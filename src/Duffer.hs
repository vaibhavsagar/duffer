{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Data.Attoparsec.ByteString hiding (parse, eitherResult)
import Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, takeTill)
import Data.Attoparsec.ByteString.Lazy (eitherResult, parse)
import Data.ByteString.Base16
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, bytestringDigest)
import Data.List (intercalate)
import Data.Set (Set, toAscList, fromList)
import Numeric (readOct)
import Prelude hiding (init, length, take)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data GitObject
    = Blob {content :: !B.ByteString}
    | Tree {entries :: !(Set TreeEntry)}
    | Commit { treeRef       :: !Ref
             , parentRefs    :: ![Ref]
             , authorTime    :: !PersonTime
             , committerTime :: !PersonTime
             , message       :: !String}
    | Tag { objectRef  :: !Ref
          , objectType :: !String
          , tagName    :: !String
          , tagger     :: !PersonTime
          , annotation :: !String}

data TreeEntry = TreeEntry !Int !B.ByteString !Ref deriving (Eq)
data PersonTime = PersonTime { personName :: !String
                             , personMail :: !String
                             , personTime :: !String
                             , personTZ   :: !String}

type Ref = B.ByteString
type Repo = String
type WithRepo = ReaderT Repo IO

instance Show GitObject where
    show object = case object of
        Blob {..} -> show content
        Tree {..} -> unlines $ map show $ toAscList entries
        Commit {..} -> concat
            [             "tree "      ~>    toString  treeRef
            , concatMap (("parent "    ~>) . toString) parentRefs
            ,             "author "    ~>    show      authorTime
            ,             "committer " ~>    show      committerTime
            ,             "\n"         ~>              message]
        Tag {..} -> concat
            [ "object " ~> toString objectRef
            , "type "   ~> objectType
            , "tag "    ~> tagName
            , "tagger " ~> show tagger
            , "\n"      ~> annotation]
        where (~>) prefix value = concat [prefix, value, "\n"] :: String

instance Show PersonTime where
    show (PersonTime name mail time tz) = concat components
        where components = [name, " <", mail, "> ", time, " ", tz]

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, toString sha1, toString name]
              octMode = printf "%06o" mode :: String
              entryType = case mode of
                0o040000 -> "tree"
                0o160000 -> "commit"
                _        -> "blob"

instance Ord TreeEntry where
    compare t1 t2 = compare (sortableName t1) (sortableName t2)
        where sortableName (TreeEntry mode name _) = name `B.append`
                if mode == 0o040000 || mode == 0o160000 then "/" else ""

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) ["objects", [sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> L.ByteString
showObject object = L.fromStrict $ uncurry makeStored $ case object of
    Blob content    -> ("blob",   content)
    Tree entries    -> ("tree",   B.concat $ map showEntry $ toAscList entries)
    commit@Commit{} -> ("commit", fromString $ show commit)
    tag@Tag{}       -> ("tag",    fromString $ show tag)
    where showEntry (TreeEntry mode name sha1) =
            let mode' = fromString $ printf "%o" mode
                sha1' = fst $ decode sha1
            in B.concat [mode', " ", name, "\NUL", sha1']

makeStored :: B.ByteString -> B.ByteString -> B.ByteString
makeStored objectType content = header `B.append` content
    where header = B.concat [objectType, " ", len, "\NUL"]
          len    = fromString . show $ B.length content

hash :: GitObject -> Ref
hash = encode . L.toStrict . bytestringDigest . sha1 . showObject

parseNull :: Parser Char
parseNull = char '\NUL'

parseHeader :: B.ByteString -> Parser String
parseHeader = (*> digit `manyTill` parseNull) . (*> space) . string

parseRestOfLine :: Parser String
parseRestOfLine = toString <$> takeTill (==10) <* endOfLine

parseMessage :: Parser String
parseMessage = endOfLine *> ((toString . B.init) <$> takeByteString)

parseRef :: Parser Ref
parseRef = take 40 <* endOfLine

parseBlob :: Parser GitObject
parseBlob = parseHeader "blob" >> Blob <$> takeByteString

parseTree :: Parser GitObject
parseTree = parseHeader "tree" >> Tree . fromList <$> many' parseTreeEntry

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = TreeEntry
    <$> (fst . head . readOct <$> digit `manyTill` space)
    <*> (takeTill (==0)       <*  parseNull)
    <*> (encode               <$> take 20)

parsePersonTime :: Parser PersonTime
parsePersonTime = PersonTime
    <$> (anyChar `manyTill` string " <")
    <*> (anyChar `manyTill` string "> ")
    <*> (digit   `manyTill` space)
    <*> parseRestOfLine

parseCommit :: Parser GitObject
parseCommit = parseHeader "commit" >> Commit
    <$>        ("tree "      *> parseRef)
    <*>  many' ("parent "    *> parseRef)
    <*>        ("author "    *> parsePersonTime)
    <*>        ("committer " *> parsePersonTime)
    <*>        parseMessage

(~~) :: GitObject -> Int -> WithRepo GitObject
(~~) object 0 = return object
(~~) object n = readObject (head $ parentRefs object) >>= \p -> p ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = readObject $ parentRefs object !! (n-1)

parseTag :: Parser GitObject
parseTag = parseHeader "tag" >> Tag
    <$> ("object " *> parseRef)
    <*> ("type "   *> parseRestOfLine)
    <*> ("tag "    *> parseRestOfLine)
    <*> ("tagger " *> parsePersonTime)
    <*> parseMessage

parseObject :: L.ByteString -> GitObject
parseObject = either error id . eitherResult . parse
    (choice [parseBlob, parseTree, parseCommit, parseTag])

readObject :: Ref -> WithRepo GitObject
readObject = (ask >>=) . ((liftIO . parseInflated).) . sha1Path
    where parseInflated = fmap (parseObject . decompress) . L.readFile

writeObject :: GitObject -> WithRepo Ref
writeObject object = asks (sha1Path sha1) >>= \path ->
    liftIO $ doesFileExist path >>= flip unless (
        createDirectoryIfMissing True (takeDirectory path) >>
        L.writeFile path ((compress . showObject) object)) >>
    return sha1 where sha1 = hash object

resolveRef :: String -> WithRepo GitObject
resolveRef = (ask >>=) . (((readObject =<<) . liftIO .
    (L.toStrict . L.init  <$>) . L.readFile) .) . flip (</>)

updateRef :: String -> GitObject -> WithRepo Ref
updateRef refPath object = asks (</> refPath) >>= liftIO . (>> return sha1) .
    flip L.writeFile (L.fromStrict $ B.append sha1 "\n")
    where sha1 = hash object
