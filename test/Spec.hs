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
import Data.ByteString.UTF8 (lines, toString)
import GHC.IO.Handle (Handle)
import Prelude hiding (lines, readFile)

import Duffer
import Duffer.Pack
import Duffer.Pack.File (resolveEntry, resolveAll')
import Duffer.Pack.Parser
import Duffer.Pack.Streaming
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
        \len objectType -> len >= 0 ==> let
            encoded = encodeTypeLen objectType len
            decoded = either error id $ parseOnly parseTypeLen encoded
            in decoded == (objectType, len :: Int)

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
testAndWriteUnpacked indexPath = describe (show indexPath) $ do
    index          <- runIO $ parsedIndex <$> readFile indexPath
    entryMap       <- runIO $ indexedEntryMap indexPath
    byteStringMap  <- runIO $ indexedByteStringMap indexPath
    it "decodes and encodes correctly" $ do
        let encodedMap =  Map.map toBytes entryMap
        encodedMap `shouldBe` byteStringMap
        let decodedMap = Map.map parsedPackRegion encodedMap
        decodedMap `shouldBe` entryMap
        let crcMap = Map.map crc32 encodedMap
        Map.elems crcMap `shouldMatchList` map getCRC index
    it "can stream a packfile" $ do
        indexedPackfile <- indexPackfile $ packFile indexPath
        indexedPackfile `shouldBe` byteStringMap
    combinedMap <- runIO $ combinedEntryMap indexPath
    it "can reconstruct the pack index entries" $
        packIndexEntries combinedMap `shouldMatchList` index
    objects <- runIO $ resolveAll  indexPath
    let refs = map (snd . toAssoc) index
    it "resolves each object correctly" $ do
        let resolvedRefs = map (\ref -> let
                Just object = resolveEntry combinedMap ref
                in hash object) refs
        resolvedRefs `shouldMatchList` refs
    it "resolves objects correctly" $ do
        let objects' = resolveAll' entryMap
        objects' `shouldMatchList` objects
        refs `shouldMatchList` map hash objects
    it "writes resolved objects out" $ do
        let write = flip runReaderT ".git" . writeObject
        mapM write objects >>= shouldMatchList refs

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
    map (combine packFilePath) . filter (\f -> takeExtension f == ".idx") <$>
    getDirectoryContents packFilePath

readHashObject :: String -> Ref -> Expectation
readHashObject path sha1 = do
    maybeObject <- runReaderT (readObject sha1) path
    case maybeObject of
        (Just object) -> hash object `shouldBe` sha1
        Nothing       -> let
            sha1String = toString sha1
            in expectationFailure $ sha1String ++ " not found"
