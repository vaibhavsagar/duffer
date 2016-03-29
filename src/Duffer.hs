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
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (openBinaryFile, IOMode(ReadMode, WriteMode), writeFile)

data GitObject = Blob {content          :: ByteString}
               | Tree {entries          :: [TreeEntry]}
               | Commit { treeRef       :: String
                        , parentRefs    :: [String]
                        , authorTime    :: String
                        , committerTime :: String
                        , message       :: String}
               deriving (Show)

data TreeEntry = TreeEntry { entryMode :: Int
                           , entryName :: String
                           , entrySha1 :: String}
                deriving (Show)

-- Given a directory and a SHA1 hash, generate a filepath
sha1Path :: String -> String -> String
sha1Path directory (s1:s2:suffix) = intercalate "/" components
    where components = [directory, "objects", [s1,s2], suffix]

-- Given a directory and a SHA1 hash, generate a directory
sha1Dir :: String -> String -> String
sha1Dir directory (s1:s2:suffix) = intercalate "/" components
    where components = [directory, "objects", [s1,s2]]

blobCreate :: String -> GitObject
blobCreate string = Blob (fromString string)

contentLength :: ByteString -> ByteString
contentLength content = fromString $ show $ length content

-- Generate a stored representation of a git object.
stored :: GitObject -> ByteString
stored object = case object of
    Blob content -> makeStored "blob" content
    Tree entries -> makeStored "tree" $ concat $ map showTreeEntry entries

makeStored :: String -> ByteString -> ByteString
makeStored objectType content =
    concat [fromString $ objectType ++ " ", contentLength content, fromString "\NUL", content]

hash :: GitObject -> String
hash object = showDigest $ sha1 $ fromStrict $ stored object

parseHeader :: ByteString -> Parser String
parseHeader object = string object >> char ' ' >> digit `manyTill` char '\NUL'

parseBlob :: Parser GitObject
parseBlob = do
    string "blob " >> digit `manyTill` char '\NUL'
    content <- takeByteString
    return $ Blob content

parseTree :: Parser GitObject
parseTree = do
    string "tree " >> digit `manyTill` char '\NUL'
    entries <- many1 treeEntry
    return $ Tree entries

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
    let modeString = fromString $ (showOct mode) ""
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
    where
        parseRef = take 40 >>= \ref -> char '\n' >> (return $ toString ref)
        parseUserTime = anyChar `manyTill` char '\n'

parseObject :: Parser GitObject
parseObject = parseBlob <|> parseTree <|> parseCommit

readObject :: String -> IO (Either String GitObject)
readObject path = do
    handle      <- openBinaryFile path ReadMode
    compressed  <- hGetContents handle
    let decompressed = toStrict $ decompress $ fromStrict compressed
    return (parseOnly parseObject decompressed)

writeObject :: String -> GitObject -> IO String
writeObject dir object = do
    let objectHash = hash object
    let path = sha1Path dir objectHash
    fileExists <- doesFileExist path
    if fileExists
        then return objectHash
        else do
            createDirectoryIfMissing True $ sha1Dir dir objectHash
            handle <- openBinaryFile path WriteMode
            hPut handle $ toStrict $ compress $ fromStrict $ stored object
            return objectHash
