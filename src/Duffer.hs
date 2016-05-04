{-# LANGUAGE OverloadedStrings #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString, length, concat, hGetContents, hPut)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.List (intercalate, nub, sortOn)
import Numeric (readOct)
import Prelude hiding (concat, length, take)
import qualified Prelude as P (concat)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (openBinaryFile, IOMode(ReadMode, WriteMode))
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
    = TreeEntry { entryMode :: Int
                , entryName :: String
                , entrySha1 :: Ref}
    deriving (Eq)

data StoredObject
    = StoredObject { repository   :: String
                   , storedObject :: GitObject}

type Ref = String
type Repo = String
type WithRepo = ReaderT Repo IO

instance Show GitObject where
    show object = case object of
        Blob content -> show content
        Tree entries -> unlines $ map show $ sortUnique entries
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
        where collate = concatMap P.concat

instance Show TreeEntry where
    show (TreeEntry mode name sha) = intercalate "\t" components
        where components = [octMode, entryType, sha, name]
              octMode = printf "%06o" mode :: String
              entryType = case octMode of
                "040000" -> "tree"
                "160000" -> "commit"
                _        -> "blob"

instance Show StoredObject where
    show (StoredObject repo object) = unlines [objectPath, show object]
        where objectPath = sha1Path repo $ hash object

-- Given a directory and a SHA1 hash, generate a directory
sha1Dir :: String -> Ref -> String
sha1Dir directory (s1:s2:_) = intercalate "/" components
    where components = [directory, "objects", [s1,s2]]
sha1Dir _ _ = error "Invalid ref provided"

-- Given a directory and a SHA1 hash, generate a filepath
sha1Path :: String -> Ref -> String
sha1Path directory ref@(_:_:suffix) = intercalate "/" components
    where components = [sha1Dir directory ref, suffix]
sha1Path _ _ = error "Invalid ref provided"

sortUnique :: [TreeEntry] -> [TreeEntry]
sortUnique = sortOn sortableName . nub

sortableName :: TreeEntry -> String
sortableName (TreeEntry mode name _) =
    if mode == 16384 || mode == 57344
        then name ++ "/"
        else name

-- Generate a stored representation of a git object.
showObject :: GitObject -> ByteString
showObject object = case object of
    Blob content    -> makeStored "blob" content
    Tree entries    -> makeStored "tree" $ concat $ map showTreeEntry $ sortUnique entries
    commit@Commit{} -> makeStored "commit" $ fromString $ show commit
    tag@Tag{}       -> makeStored "tag" $ fromString $ show tag

makeStored :: String -> ByteString -> ByteString
makeStored objectType content = concat [header, content]
    where header = fromString $ P.concat [objectType, " ", len, "\NUL"]
          len    = show $ length content

hash :: GitObject -> Ref
hash object = showDigest $ sha1 $ fromStrict $ showObject object

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
    return $ readTreeEntry mode filename sha1

readTreeEntry :: String -> String -> ByteString -> TreeEntry
readTreeEntry modeString filenameString sha1String =
    let mode     = fst $ head $ readOct modeString
        filename = filenameString
        sha1     = toString $ encode sha1String
    in TreeEntry mode filename sha1

showTreeEntry :: TreeEntry -> ByteString
showTreeEntry (TreeEntry mode name sha1) =
    let modeString = fromString $ printf "%o" mode
        nameString = fromString name
        sha1String = fst $ decode $ fromString sha1
    in concat [modeString, " ", nameString, "\NUL", sha1String]

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
-- (~~) object 0 = return object
(~~) object n = ask >>= \repo -> do
    let ref = head $ parentRefs object
    readObject ref >>= \parent -> parent ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = ask >>= \repo -> do
    let ref = parentRefs object !! (n-1)
    readObject ref

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
readObject sha1 = ask >>= \repo ->
    liftIO (decompressed $ sha1Path repo sha1) >>= \decompressed ->
    return $ either error id $ parseOnly parseObject decompressed

decompressed :: String -> IO ByteString
decompressed path = do
    handle      <- openBinaryFile path ReadMode
    compressed  <- hGetContents handle
    return $ toStrict $ decompress $ fromStrict compressed

writeObject :: GitObject -> WithRepo Ref
writeObject object = ask >>= \repo -> do
    let sha1 = hash object
    let path = sha1Path repo sha1
    liftIO $ doesFileExist path >>= \fileExists -> unless fileExists $ do
        createDirectoryIfMissing True $ sha1Dir repo sha1
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
updateRef refPath object = ask >>= \repo -> do
    let sha1 = hash object
    let path = intercalate "/" [repo, refPath]
    liftIO $ do handle <- openBinaryFile path WriteMode
                hPut handle $ fromString $ sha1 ++ "\n"
    return sha1
