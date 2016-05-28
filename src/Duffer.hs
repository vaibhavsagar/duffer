{-# LANGUAGE OverloadedStrings #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad (join, unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import Data.ByteString (ByteString, append, init, length, readFile, writeFile)
import qualified Data.ByteString as B (concat)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, bytestringDigest)
import Data.List (intercalate, nub, sortOn)
import Numeric (readOct)
import Prelude hiding (init, length, readFile, writeFile, take, null)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.Printf (printf)

data GitObject
    = Blob {content :: ByteString}
    | Tree {entries :: [TreeEntry]}
    | Commit { treeRef       :: Ref
             , parentRefs    :: [Ref]
             , authorTime    :: String
             , committerTime :: String
             , message       :: String}
    | Tag { objectRef  :: Ref
          , objectType :: String
          , tagName    :: String
          , tagger     :: String
          , annotation :: String}

data TreeEntry = TreeEntry Int ByteString Ref deriving (Eq)

type Ref = ByteString
type Repo = String
type WithRepo = ReaderT Repo IO

instance Show GitObject where
    show object = case object of
        Blob content -> show content
        Tree entries -> unlines $ map show $ sortEntries entries
        Commit treeRef parentRefs authorTime committerTime message ->
            concat [    "tree "      `is`    toString  treeRef
                   , concatMap (
                       ("parent "    `is`) . toString) parentRefs
                   ,    "author "    `is`              authorTime
                   ,    "committer " `is`              committerTime
                   ,    "\n"         `is`              message]
        Tag objectRef objectType tagName tagger annotation ->
            concat [ "object " `is` toString objectRef
                   , "type "   `is` objectType
                   , "tag "    `is` tagName
                   , "tagger " `is` tagger
                   , "\n"      `is` annotation]
        where is prefix value = concat [prefix, value, "\n"] :: String

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, toString sha1, toString name]
              octMode = printf "%06o" mode :: String
              entryType = case octMode of
                "040000" -> "tree"
                "160000" -> "commit"
                _        -> "blob"

sha1Path :: Repo -> Ref -> FilePath
sha1Path repo ref = let (sa:sb:suffix) = toString ref in
    repo </> "objects" </> [sa, sb] </> suffix

sortEntries :: [TreeEntry] -> [TreeEntry]
sortEntries = sortOn sortableName . nub
    where sortableName (TreeEntry mode name _) =
            B.concat [name, if mode == 16384 || mode == 57344 then "/" else ""]

-- Generate a stored representation of a git object.
showObject :: GitObject -> ByteString
showObject object = uncurry makeStored $ case object of
    Blob content    -> ("blob", content)
    Tree _          -> ("tree", B.concat $ map showTreeEntry sortedEntries)
    commit@Commit{} -> ("commit", fromString $ show commit)
    tag@Tag{}       -> ("tag", fromString $ show tag)
    where sortedEntries = sortEntries $ entries object
          showTreeEntry (TreeEntry mode name sha1) =
            let modeString = fromString $ printf "%o" mode
                sha1String = fst $ decode sha1
            in B.concat [modeString, " ", name, "\NUL", sha1String]

makeStored :: ByteString -> ByteString -> ByteString
makeStored objectType content = header `append` content
    where header = B.concat [objectType, " ", len, "\NUL"]
          len    = fromString . show $ length content

hash :: GitObject -> Ref
hash = encode . toStrict . bytestringDigest . sha1 . fromStrict . showObject

null :: Parser Char
null = char '\NUL'

parseHeader :: ByteString -> Parser String
parseHeader = (>> digit `manyTill` char '\NUL') . (>> char ' ') . string

parseRef :: Parser Ref
parseRef = take 40 <* endOfLine

parseBlob :: Parser GitObject
parseBlob = parseHeader "blob" >>
    takeByteString >>= \content -> return $ Blob content

parseTree :: Parser GitObject
parseTree = parseHeader "tree" >>
    many' parseTreeEntry >>= \entries -> return $ Tree entries

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    mode <- (fst . head . readOct) <$> digit `manyTill` space
    name <- takeTill (==0) <* null
    sha1 <- encode <$> take 20
    return $ TreeEntry mode name sha1

parseCommit :: Parser GitObject
parseCommit = parseHeader "commit" *> do
    treeRef <-       "tree "      *> parseRef
    parentRefs <- many' (
                     "parent "    *> parseRef)
    authorTime <-    "author "    *> restOfLine
    committerTime <- "committer " *> restOfLine
    endOfLine
    message <- (toString . init) <$> takeByteString
    return $ Commit treeRef parentRefs authorTime committerTime message
    where restOfLine = toString <$> takeTill (==10) <* "\n"

(~~) :: GitObject -> Int -> WithRepo GitObject
(~~) object 0 = return object
(~~) object n = readObject (head $ parentRefs object) >>= \p -> p ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = readObject $ parentRefs object !! (n-1)

parseTag :: Parser GitObject
parseTag = parseHeader "tag" *> do
    objectRef  <- "object " *> parseRef
    objectType <- "type "   *> restOfLine
    tagName    <- "tag "    *> restOfLine
    tagger     <- "tagger " *> restOfLine
    endOfLine
    annotation <- (toString . init) <$> takeByteString
    return $ Tag objectRef objectType tagName tagger annotation
    where restOfLine = toString <$> takeTill (==10) <* "\n"

parseObject :: Parser GitObject
parseObject = choice [parseBlob, parseTree, parseCommit, parseTag]

readObject :: Ref -> WithRepo GitObject
readObject = (ask >>=) . ((fmap (either error id . parseOnly parseObject) .
    liftIO . inflated) .) . flip sha1Path
    where inflated = fmap (toStrict . decompress . fromStrict) . readFile

writeObject :: GitObject -> WithRepo Ref
writeObject object = ask >>= \repo -> do
    let sha1 = hash object
    let path = sha1Path repo sha1
    liftIO $ doesFileExist path >>= \fileExists -> unless fileExists $ do
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path $ toStrict $ compress $ fromStrict $ showObject object
    return sha1

resolveRef :: String -> WithRepo GitObject
resolveRef = (ask >>=) . (((readObject =<<) . liftIO . (init <$>)
    . readFile) .) . flip (</>)

updateRef :: String -> GitObject -> WithRepo Ref
updateRef refPath object = let sha1 = hash object in ask >>= liftIO .
    (>> return sha1) . flip writeFile (sha1 `append` "\n") . (</> refPath)
