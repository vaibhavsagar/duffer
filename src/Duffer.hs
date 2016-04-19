{-# LANGUAGE OverloadedStrings #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString, length, concat, pack, unpack, hGetContents, hPut)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.List (intercalate, nub, sortOn)
import Numeric (showHex, readHex, showOct, readOct)
import Prelude hiding (concat, length, take)
import qualified Prelude as P (concat)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (openBinaryFile, IOMode(ReadMode, WriteMode), writeFile)
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
    deriving (Show)

data TreeEntry
    = TreeEntry { entryMode :: Int
                , entryName :: String
                , entrySha1 :: Ref}
    deriving (Show, Eq)

data StoredObject
    = StoredObject { repository   :: String
                   , storedObject :: GitObject}
    deriving (Show)

type Ref = String

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
    Blob content -> makeStored "blob" content
    Tree entries    -> makeStored "tree" $ concat $ map showTreeEntry $ sortUnique entries
    Commit treeRef parentRefs authorTime committerTime message ->
        let treeLine      = ["tree ", treeRef, "\n"]
            parentLines   = map (\ref -> "parent " ++ ref ++ "\n") parentRefs
            authorLine    = ["author ", authorTime, "\n"]
            committerLine = ["committer ", committerTime, "\n"]
            messageLines  = ["\n", message, "\n"]
            content       = collate [treeLine, parentLines, authorLine, committerLine, messageLines]
        in makeStored "commit" content
    Tag objectRef objectType tagName tagger annotation ->
        let objectLine = ["object ", objectRef, "\n"]
            typeLine   = ["type ", objectType, "\n"]
            tagLine    = ["tag ", tagName, "\n"]
            taggerLine = ["tagger ", tagger, "\n"]
            annotLines = ["\n", annotation, "\n"]
            content    = collate [objectLine, typeLine, tagLine, taggerLine, annotLines]
        in makeStored "tag" content
    where collate = fromString . concatMap P.concat

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

(~~) :: StoredObject -> Int -> IO StoredObject
(~~) object 0 = return object
(~~) object n =
    let ref = head $ parentRefs $ storedObject object
    in readObject (repository object) ref >>= \parent ->
    parent ~~ (n-1)

(^^) :: StoredObject -> Int -> IO StoredObject
(^^) object n =
    let ref = parentRefs (storedObject object) !! (n-1)
    in readObject (repository object) ref

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

readObject :: String -> Ref -> IO StoredObject
readObject repo sha1 = do
    let path = sha1Path repo sha1
    handle      <- openBinaryFile path ReadMode
    compressed  <- hGetContents handle
    let decompressed = toStrict $ decompress $ fromStrict compressed
        parsed       = parseOnly parseObject decompressed
    return $ either error (StoredObject repo) parsed

writeObject :: StoredObject -> IO String
writeObject (StoredObject dir object) =
    let sha1 = hash object
        path = sha1Path dir sha1
    in doesFileExist path >>= \fileExists ->
    if fileExists then return sha1 else do
        createDirectoryIfMissing True $ sha1Dir dir sha1
        handle <- openBinaryFile path WriteMode
        hPut handle $ toStrict $ compress $ fromStrict $ showObject object
        return sha1

resolveRef :: String -> String -> IO StoredObject
resolveRef repo refPath = do
    let path = intercalate "/" [repo, refPath]
    handle <- openBinaryFile path ReadMode
    sha1   <- hGetContents handle
    readObject repo $ init $ toString sha1

updateRef :: String -> StoredObject -> IO String
updateRef refPath (StoredObject repo object) =
    let sha1 = hash object
        path = intercalate "/" [repo, refPath]
    in do handle <- openBinaryFile path WriteMode
          hPut handle $ fromString $ sha1 ++ "\n"
          return sha1
