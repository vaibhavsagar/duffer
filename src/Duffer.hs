{-# LANGUAGE OverloadedStrings #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad (join, unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString, length, hGetContents, hPut)
import qualified Data.ByteString as B (concat)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.List (intercalate, nub, sortOn)
import Numeric (readOct)
import Prelude hiding (length, take)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (openBinaryFile, IOMode(..))
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

data TreeEntry
    = TreeEntry { entryMode :: Int, entryName :: String, entrySha1 :: Ref}
    deriving (Eq)

type Ref = String
type Repo = String
type WithRepo = ReaderT Repo IO

instance Show GitObject where
    show object = case object of
        Blob content -> show content
        Tree entries -> unlines $ map show $ sortEntries entries
        Commit treeRef parentRefs authorTime committerTime message ->
            let treeLine      = ["tree ", treeRef, "\n"]
                parentLines   = map (\ref -> "parent " ++ ref ++ "\n") parentRefs
                authorLine    = ["author ", authorTime, "\n"]
                committerLine = ["committer ", committerTime, "\n"]
                messageLines  = ["\n", message, "\n"]
                content       = [treeLine, parentLines, authorLine, committerLine, messageLines]
            in collate content
        Tag objectRef objectType tagName tagger annotation ->
            let objectLine = ["object ", objectRef, "\n"]
                typeLine   = ["type ", objectType, "\n"]
                tagLine    = ["tag ", tagName, "\n"]
                taggerLine = ["tagger ", tagger, "\n"]
                annotLines = ["\n", annotation, "\n"]
                content    = [objectLine, typeLine, tagLine, taggerLine, annotLines]
            in collate content
        where collate = concatMap concat

instance Show TreeEntry where
    show (TreeEntry mode name sha) = intercalate "\t" components
        where components = [octMode, entryType, sha, name]
              octMode = printf "%06o" mode :: String
              entryType = case octMode of
                "040000" -> "tree"
                "160000" -> "commit"
                _        -> "blob"

sha1Path :: Repo -> Ref -> FilePath
sha1Path repo (sa:sb:suffix) = repo </> "objects" </> [sa, sb] </> suffix
sha1Path _ _ = error "Invalid ref provided"

sortEntries :: [TreeEntry] -> [TreeEntry]
sortEntries = sortOn sortableName . nub
    where sortableName (TreeEntry mode name _) =
            name ++ (if mode == 16384 || mode == 57344 then "/" else "")

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
                nameString = fromString name
                sha1String = fst $ decode $ fromString sha1
            in B.concat [modeString, " ", nameString, "\NUL", sha1String]

makeStored :: String -> ByteString -> ByteString
makeStored objectType content = B.concat [header, content]
    where header = fromString $ concat [objectType, " ", len, "\NUL"]
          len    = show $ length content

hash :: GitObject -> Ref
hash = showDigest . sha1 . fromStrict . showObject

parseHeader :: ByteString -> Parser String
parseHeader object = string object >> char ' ' >> digit `manyTill` char '\NUL'

parseRef :: Parser Ref
parseRef = take 40 >>= \ref -> char '\n' >> return (toString ref)

parseBlob :: Parser GitObject
parseBlob = parseHeader "blob" >>
    takeByteString >>= \content -> return $ Blob content

parseTree :: Parser GitObject
parseTree = parseHeader "tree" >>
    many' treeEntry >>= \entries -> return $ Tree entries

treeEntry :: Parser TreeEntry
treeEntry = do
    mode     <- digit `manyTill` space
    filename <- anyChar `manyTill` char '\NUL'
    sha1     <- take 20
    return $
        TreeEntry (fst $ head $ readOct mode) filename (toString $ encode sha1)

parseCommit :: Parser GitObject
parseCommit = parseHeader "commit" >> do
    string "tree "; treeRef <- parseRef
    parentRefs <- many' (string "parent " >> parseRef)
    string "author ";    authorTime    <- parseUserTime
    string "committer "; committerTime <- parseUserTime
    char '\n'
    msg <- takeByteString
    let message = init $ toString msg
    return $ Commit treeRef parentRefs authorTime committerTime message
    where parseUserTime = anyChar `manyTill` char '\n'

(~~) :: GitObject -> Int -> WithRepo GitObject
(~~) object 0 = return object
(~~) object n = readObject (head $ parentRefs object) >>= \p -> p ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = readObject $ parentRefs object !! (n-1)

parseTag :: Parser GitObject
parseTag = parseHeader "tag" >> do
    string "object "; objectRef <- parseRef
    string "type ";   objectType <- restOfLine
    string "tag ";    tagName <- restOfLine
    string "tagger "; tagger <- restOfLine
    char '\n'
    ann <- takeByteString
    let annotation = init $ toString ann
    return $ Tag objectRef objectType tagName tagger annotation
    where restOfLine = anyChar `manyTill` char '\n'

parseObject :: Parser GitObject
parseObject = choice [parseBlob, parseTree, parseCommit, parseTag]

readObject :: Ref -> WithRepo GitObject
readObject = (ask >>=) . ((fmap (either error id . parseOnly parseObject) .
    liftIO . decompressed) .) . flip sha1Path

decompressed :: String -> IO ByteString
decompressed = fmap (toStrict . decompress . fromStrict) . fileContents

fileContents :: String -> IO ByteString
fileContents = join . fmap hGetContents . (`openBinaryFile` ReadMode)

writeObject :: GitObject -> WithRepo Ref
writeObject object = ask >>= \repo -> do
    let sha1 = hash object
    let path = sha1Path repo sha1
    liftIO $ doesFileExist path >>= \fileExists -> unless fileExists $ do
        createDirectoryIfMissing True $ takeDirectory path
        handle <- openBinaryFile path WriteMode
        hPut handle $ toStrict $ compress $ fromStrict $ showObject object
    return sha1

resolveRef :: String -> WithRepo GitObject
resolveRef refPath = ask >>= \repo -> do
    let path = intercalate "/" [repo, refPath]
    sha1 <- liftIO $ openBinaryFile path ReadMode >>=
        (hGetContents >=> \content -> return $ init $ toString content)
    readObject sha1

updateRef :: String -> GitObject -> WithRepo Ref
updateRef refPath object = ask >>= \repo ->
    let sha1 = hash object
        path = intercalate "/" [repo, refPath]
    in liftIO $ do handle <- openBinaryFile path WriteMode
                   hPut handle $ fromString $ sha1 ++ "\n"
                   return sha1
