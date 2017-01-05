{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad              (zipWithM_)
import Data.Aeson                 (encode, decode)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString            (readFile, hGetContents, split)
import Data.ByteString.UTF8       (lines, toString)
import Data.Byteable              (Byteable(..))
import Data.Digest.CRC32          (crc32)
import Data.IntMap.Strict         (elems)
import Data.Maybe                 (fromJust)
import GHC.IO.Handle              (Handle)
import System.FilePath            ()
import System.Process             (CreateProcess(..), StdStream(..)
                                  ,createProcess, shell)
import Test.Hspec                 (hspec, parallel, expectationFailure
                                  ,describe, it, runIO, shouldBe
                                  ,shouldMatchList, Expectation, SpecWith)
import Test.QuickCheck            (Arbitrary(..), oneof, property, (==>))

import Prelude hiding (lines, readFile)

import Duffer.Unified        (readRef, readObject, writeObject)
import Duffer.Loose.Objects  (GitObject, Ref, hash)
import Duffer.Pack           (getPackIndices, indexedEntryMap
                             ,indexedByteStringMap, packFile, combinedEntryMap
                             ,resolveAll)
import Duffer.Pack.File      (resolveEntry, resolveAll')
import Duffer.Pack.Parser    (parseOffset, parseTypeLen, parsedIndex
                             ,parsedPackRegion)
import Duffer.Pack.Streaming (indexPackFile)
import Duffer.Pack.Entries
import Duffer.WithRepo       (withRepo)

main :: IO ()
main = do
    testEncodingAndParsing
    let objectTypes = ["blob", "tree", "commit", "tag"]
    allTypesObjects <- mapM objectsOfType objectTypes
    testReading objectTypes allTypesObjects
    testUnpackingAndWriting =<< getPackIndices ".git/objects"
    testReading objectTypes allTypesObjects
    testJSON    objectTypes allTypesObjects
    testRefs

newtype TestPackObjectType
    = TestPackObjectType { innerPackObject :: PackObjectType }
    deriving (Show)

instance Arbitrary TestPackObjectType where
    arbitrary = oneof $ map (return . TestPackObjectType)
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
            encoded = encodeTypeLen (innerPackObject objectType) len
            decoded = either error id $ parseOnly parseTypeLen encoded
            in decoded == (innerPackObject objectType, len :: Int)

testUnpackingAndWriting :: [FilePath] -> IO ()
testUnpackingAndWriting indices =
    hspec . parallel $ describe "unpacking packfiles" $
        mapM_ testAndWriteUnpacked indices

testReading :: [String] -> [[Ref]] -> IO ()
testReading types partitionedObjects =
    hspec . parallel $ describe "reading objects" $
        zipWithM_ describeReadingAll types partitionedObjects

describeReadingAll :: String -> [Ref] -> SpecWith ()
describeReadingAll oType objects = describe oType $
    readAll ("correctly parses and hashes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (readHashObject ".git") os)

testJSON :: [String] -> [[Ref]] -> IO ()
testJSON types partitionedObjects =
    hspec . parallel $ describe "decoding and encoding" $
        zipWithM_ describeDecodingEncodingAll types partitionedObjects

testRefs :: IO ()
testRefs = hspec . parallel $ describe "reading refs" $ do
    refsOutput <- runIO allRefs
    it "correctly reads refs" $
        mapM_ (checkRef ".git") refsOutput
    where checkRef repo (path, ref) = withRepo repo (readRef path) >>= \case
            (Just someRef) -> someRef `shouldBe` ref
            Nothing        -> expectationFailure $ path ++ " not found"

describeDecodingEncodingAll :: String -> [Ref] -> SpecWith ()
describeDecodingEncodingAll oType objects = describe oType $
    readAll ("correctly decodes and encodes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (decodeEncodeObject ".git") os)

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = describe (show indexPath) $ do
    index          <- runIO $ parsedIndex <$> readFile indexPath
    entryMap       <- runIO $ indexedEntryMap indexPath
    byteStringMap  <- runIO $ indexedByteStringMap indexPath
    it "decodes and encodes pack entries correctly" $ do
        let encodedMap =  fmap toBytes entryMap
        encodedMap `shouldBe` byteStringMap
        let decodedMap = fmap parsedPackRegion encodedMap
        decodedMap `shouldBe` entryMap
        let crcMap = fmap crc32 encodedMap
        elems crcMap `shouldMatchList` map pieCRC index
    it "can separate a streamed packfile" $ do
        indexedPackFile <- indexPackFile $ packFile indexPath
        indexedPackFile `shouldBe` byteStringMap
    combinedMap <- runIO $ combinedEntryMap indexPath
    it "can reconstruct the pack index entries" $
        packIndexEntries combinedMap `shouldMatchList` index
    objects <- runIO $ resolveAll  indexPath
    let refs = map (snd . toAssoc) index
    it "resolves each object correctly" $ do
        let resolvedRefs = map (hash . fromJust . resolveEntry combinedMap) refs
        resolvedRefs `shouldMatchList` refs
    it "resolves objects correctly" $ do
        let objects' = resolveAll' entryMap
        objects' `shouldMatchList` objects
        refs     `shouldMatchList` map hash objects
    it "writes resolved objects out" $ do
        let write = withRepo ".git" . writeObject
        mapM write objects >>= shouldMatchList refs

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = fmap lines $
    cmd "git rev-list --objects --all"
    >|> "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'"
    >|> ("grep '^[^ ]* " ++ objectType ++ "'")
    >|> "cut -d' ' -f1"
    >>= hGetContents

allRefs :: IO [(FilePath, Ref)]
allRefs = do
    content  <- cmd "git show-ref" >>= hGetContents
    let refs =  split 32 <$> lines content
    return $ map (\[p, h] -> (toString h, p)) refs

cmd :: String -> IO Handle
cmd command = createProcess (shell command) {std_out = CreatePipe} >>=
    \(_, Just handle, _, _) -> return handle

(>|>) :: IO Handle -> String -> IO Handle
(>|>) handle command = withPipe =<< handle
    where withPipe pipe = createProcess (shell command)
            {std_out = CreatePipe, std_in = UseHandle pipe} >>=
            \(_, Just handle', _, _) -> return handle'

readHashObject :: String -> Ref -> Expectation
readHashObject path sha1 = withRepo path (readObject sha1) >>= \case
    (Just object) -> hash object `shouldBe` sha1
    Nothing       -> let
        sha1String = toString sha1
        in expectationFailure $ sha1String ++ " not found"

decodeEncodeObject :: FilePath -> Ref -> Expectation
decodeEncodeObject path ref = withRepo path (readObject ref) >>= \case
    (Just object) -> let
        encoded = encode object
        decoded = fromJust $ decode encoded :: GitObject
        in decoded `shouldBe` object
    Nothing       -> let
        sha1String = toString ref
        in expectationFailure $ sha1String ++ " not found"
