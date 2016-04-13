{-# LANGUAGE OverloadedStrings #-}

module Duffer where

import Codec.Compression.Zlib (compress, decompress)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString, length, concat, pack, unpack, hGetContents, hPut)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.List (intercalate)
import Numeric (showHex, readHex, showOct, readOct)
import Prelude hiding (concat, length, take)
import qualified Prelude as P (concat)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (openBinaryFile, IOMode(ReadMode, WriteMode), writeFile)

data GitObject = Blob {content          :: ByteString}
               | Tree {entries          :: [TreeEntry]}
               | Commit { treeRef       :: Ref
                        , parentRefs    :: [Ref]
                        , authorTime    :: String
                        , committerTime :: String
                        , message       :: String}
               deriving (Show)

data TreeEntry = TreeEntry { entryMode :: Int
                           , entryName :: String
                           , entrySha1 :: Ref}
                deriving (Show)

data StoredObject = StoredObject { repository   :: String
                                 , storedObject :: GitObject}
                  deriving (Show)

type Ref = String

-- Given a directory and a SHA1 hash, generate a directory
sha1Dir :: String -> Ref -> String
sha1Dir directory (s1:s2:_) = intercalate "/" components
    where components = [directory, "objects", [s1,s2]]

-- Given a directory and a SHA1 hash, generate a filepath
sha1Path :: String -> Ref -> String
sha1Path directory sha1@(_:_:suffix) = intercalate "/" components
    where components = [sha1Dir directory sha1, suffix]

-- Generate a stored representation of a git object.
stored :: GitObject -> ByteString
stored object = case object of
    Blob content -> makeStored "blob" content
    Tree entries -> makeStored "tree" $ concat $ map showTreeEntry entries
    Commit treeRef parentRefs authorTime committerTime message ->
        let treeLine      = ["tree ", treeRef, "\n"]
            parentLines   = map (\ref -> "parent " ++ ref ++ "\n") parentRefs
            authorLine    = ["author ", authorTime, "\n"]
            committerLine = ["committer ", committerTime, "\n"]
            messageLines  = ["\n", message, "\n"]
            content       = collate [treeLine, parentLines, authorLine, committerLine, messageLines]
        in makeStored "commit" content
    where collate = concat . map (fromString . P.concat)

makeStored :: String -> ByteString -> ByteString
makeStored objectType content = concat [header, content]
    where header = concat $ map fromString [objectType, " ", show $ length content, "\NUL"]

hash :: GitObject -> Ref
hash object = showDigest $ sha1 $ fromStrict $ stored object

parseHeader :: ByteString -> Parser String
parseHeader object = string object >> char ' ' >> digit `manyTill` char '\NUL'

parseRef :: Parser Ref
parseRef = take 40 >>= \ref -> char '\n' >> return (toString ref)

parseBlob :: Parser GitObject
parseBlob = parseHeader "blob" >>
    takeByteString >>= \content -> return $ Blob content

parseTree :: Parser GitObject
parseTree = parseHeader "tree" >>
    many1 treeEntry >>= \entries -> return $ Tree entries

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
    let modeString = fromString $ showOct mode ""
        nameString = fromString name
        sha1String = fst $ decode $ fromString sha1
        entryString = concat [modeString, " ", nameString, "\NUL", sha1String]
    in entryString

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

parseObject :: Parser GitObject
parseObject = parseBlob <|> parseTree <|> parseCommit

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
        hPut handle $ toStrict $ compress $ fromStrict $ stored object
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
    in do
        handle <- openBinaryFile path WriteMode
        hPut handle $ fromString $ sha1 ++ "\n"
        return sha1
