{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as Map

import Data.Attoparsec.ByteString (parseOnly)
import Data.Byteable
import Data.ByteString (readFile, hGetContents)
import Data.Digest.CRC32
import Test.Hspec
import Test.QuickCheck
import Control.Monad (zipWithM_)
import System.Process
import System.FilePath
import System.Directory
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.UTF8 (lines)
import GHC.IO.Handle (Handle)
import Prelude hiding (lines, readFile)

import Duffer
import Duffer.Pack
import Duffer.Pack.File (resolveAll')
import Duffer.Pack.Parser
import Duffer.Pack.Entries

main :: IO ()
main = do
    testEncodingAndParsing
    testUnpackingAndWriting =<< getPackIndices ".git"
    let objectTypes = ["blob", "tree", "commit", "tag"]
    testReadingLoose objectTypes =<< mapM objectsOfType objectTypes

instance Arbitrary PackObjectType where
    arbitrary = oneof $ map return
        [ CommitObject
        , TreeObject
        , BlobObject
        , TagObject
        , OfsDeltaObject
        , RefDeltaObject
        ]

testEncodingAndParsing :: IO ()
testEncodingAndParsing = hspec . parallel $ describe "integer encodings" $ do
    it "encodes and decodes offsets" $ property $
        \offset -> offset >= 0 ==> let
            encoded = encodeOffset offset
            decoded = either error id $ parseOnly parseOffset encoded
            in decoded == (offset :: Int)
    it "encodes and decodes object types and lengths" $ property $
        \len oType -> len >= 0 ==> let
            encoded = encodeTypeLen oType len
            decoded = either error id $ parseOnly parseTypeLen encoded
            in decoded == (oType, len :: Int)

testUnpackingAndWriting :: [FilePath] -> IO ()
testUnpackingAndWriting indices =
    hspec . parallel $ describe "unpacking packfiles" $
        mapM_ testAndWriteUnpacked indices

testReadingLoose :: [String] -> [[Ref]] -> IO ()
testReadingLoose types partitionedObjects =
    hspec . parallel $ describe "reading loose objects" $
        zipWithM_ describeReadingAll types partitionedObjects

describeReadingAll :: String -> [Ref] -> SpecWith ()
describeReadingAll oType objects = describe oType $
    readAll ("correctly parses and hashes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (readHashObject ".git") os)

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = it (show indexPath) $ do
    index     <- parsedIndex <$> readFile        indexPath
    let refs   =  map (snd . toAssoc)          index
    let crcs   =  map getCRC index
    iEMap     <-      indexedEntryMap indexPath
    iBSMap    <- indexedByteStringMap indexPath
    cEMap     <- combinedEntryMap     indexPath
    let iEnMap  = Map.map toBytes iEMap
    let iDecMap = Map.map parsedPackRegion iEnMap
    let crcMap  = Map.map crc32 iEnMap

    shouldBe iEnMap iBSMap
    shouldBe iDecMap iEMap
    shouldMatchList (Map.elems crcMap) crcs

    objects       <- resolveAll indexPath
    let objects'  =  resolveAll' iEMap
    let write     =  flip runReaderT ".git" . writeObject

    shouldMatchList objects'            objects
    shouldMatchList refs (map hash      objects)
    shouldMatchList refs =<< mapM write objects
    shouldMatchList (Map.elems iDecMap) (Map.elems iEMap)
    shouldMatchList (packIndexEntries cEMap) index

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = fmap lines $
    cmd "git rev-list --objects --all"
    >|> "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'"
    >|> ("grep '^[^ ]* " ++ objectType ++ "'")
    >|> "cut -d' ' -f1"
    >>= hGetContents

cmd :: String -> IO Handle
cmd command = createProcess (shell command) {std_out = CreatePipe} >>=
    \(_, Just handle, _, _) -> return handle

(>|>) :: IO Handle -> String -> IO Handle
(>|>) handle cmd = withPipe cmd =<< handle
    where withPipe cmd pipe = createProcess (shell cmd)
            {std_out = CreatePipe, std_in = UseHandle pipe} >>=
            \(_, Just handle, _, _) -> return handle

getPackIndices :: String -> IO [FilePath]
getPackIndices path = let packFilePath = path ++ "/objects/pack" in
    map (combine packFilePath) .  filter (\f -> takeExtension f == ".idx") <$>
    getDirectoryContents packFilePath

readHashObject :: String -> Ref -> Expectation
readHashObject path sha1 =
    hash <$> runReaderT (readObject sha1) path `shouldReturn` sha1
