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
import Test.Hspec                 (hspec, expectationFailure, describe
                                  ,it, runIO, shouldBe, shouldMatchList
                                  ,Expectation, SpecWith)
import Test.QuickCheck            (Arbitrary(..), oneof, property, (==>))

import Prelude hiding (lines, readFile)

import Duffer.Unified        (readRef, readObject, writeObject)
import Duffer.Loose.Objects  (GitObject, Ref, hash)
import Duffer.Pack           (getPackIndices, indexedEntryMap
                             ,indexedByteStringMap, packFile, combinedEntryMap
                             ,resolveAll)
import Duffer.Pack.File      (resolveAll')
import Duffer.Pack.Parser    (parseOffset, parseTypeLen, parsedIndex
                             ,parsedPackRegion)
import Duffer.Pack.Streaming (separatePackFile)
import Duffer.Pack.Entries   (PackObjectType(..), encodeOffset, encodeTypeLen
                             ,PackIndexEntry(..), packIndexEntries, toAssoc
                             ,FullObjectType(..), DeltaObjectType(..))
import Duffer.WithRepo       (withRepo)
import Duffer.WorkObject

main :: IO ()
main = do
    let objectTypes = ["blob", "tree", "commit", "tag"]
    allTypesObjects <- mapM objectsOfType objectTypes
    hspec $ testEncodingAndParsing
    hspec $ testReading "packed"   objectTypes allTypesObjects
    hspec . testUnpackingAndWriting =<< getPackIndices ".git/objects"
    hspec $ testReading "unpacked" objectTypes allTypesObjects
    hspec $ testJSON               objectTypes allTypesObjects
    hspec . testRefs =<< allRefs
    hspec . testWorkTrees =<< objectsOfType "tree"

newtype TestPackObjectType
    = TestPackObjectType { innerPackObject :: PackObjectType }
    deriving (Show)

instance Arbitrary TestPackObjectType where
    arbitrary = oneof $ map (return . TestPackObjectType)
        [ FullType  CommitType
        , FullType  TreeType
        , FullType  BlobType
        , FullType  TagType
        , DeltaType OfsDeltaType
        , DeltaType RefDeltaType
        ]

testEncodingAndParsing :: SpecWith ()
testEncodingAndParsing = describe "integer encodings" $ do
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

testUnpackingAndWriting :: [FilePath] -> SpecWith ()
testUnpackingAndWriting indices = describe "unpacking packfiles" $
    mapM_ testAndWriteUnpacked indices

testReading :: String -> [String] -> [[Ref]] -> SpecWith ()
testReading status types partitionedObjects =
    describe ("reading " ++ status ++ " objects") $
        zipWithM_ describeReadingAll types partitionedObjects

describeReadingAll :: String -> [Ref] -> SpecWith ()
describeReadingAll oType objects = describe oType $
    readAll ("correctly parses and hashes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (readHashObject ".git") os)

testJSON :: [String] -> [[Ref]] -> SpecWith ()
testJSON types partitionedObjects = describe "decoding and encoding" $
    zipWithM_ describeDecodingEncodingAll types partitionedObjects

testWorkTrees :: [Ref] -> SpecWith ()
testWorkTrees refs = describe "work trees" $
    it "reads and hashes WorkObjects" (mapM_ (readHashWorkObject ".git") refs)

testRefs :: [(FilePath, Ref)] -> SpecWith ()
testRefs refsOutput = describe "reading refs" $
    it "correctly reads refs" $ mapM_ (checkRef ".git") refsOutput
    where checkRef repo (path, ref) = withRepo repo (readRef path) >>= maybe
            (failureNotFound path)
            (`shouldBe` ref)

describeDecodingEncodingAll :: String -> [Ref] -> SpecWith ()
describeDecodingEncodingAll oType objects = describe oType $
    readAll ("correctly decodes and encodes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (decodeEncodeObject ".git") os)

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = describe (show indexPath) $ do
    index    <- runIO $ parsedIndex <$> readFile indexPath
    entryMap <- runIO $ indexedEntryMap indexPath
    it "decodes and encodes pack entries correctly" $ do
        byteStringMap  <- indexedByteStringMap indexPath
        let encodedMap =  fmap toBytes entryMap
        encodedMap `shouldBe` byteStringMap
        let decodedMap = fmap parsedPackRegion encodedMap
        decodedMap `shouldBe` entryMap
        let crcMap = fmap crc32 encodedMap
        elems crcMap `shouldMatchList` map pieCRC index
    it "can separate a streamed packfile" $ do
        separatedPackFile <- separatePackFile $ packFile indexPath
        separatedPackFile `shouldBe` entryMap
    combinedMap <- runIO $ combinedEntryMap indexPath
    it "can reconstruct the pack index entries" $
        packIndexEntries combinedMap `shouldMatchList` index
    let objects = resolveAll combinedMap
    let refs = map (snd . toAssoc) index
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
readHashObject path sha1 = withRepo path (readObject sha1) >>= maybe
    (failureNotFound $ toString sha1)
    (\object -> hash object `shouldBe` sha1)

decodeEncodeObject :: FilePath -> Ref -> Expectation
decodeEncodeObject path ref = withRepo path (readObject ref) >>= maybe
    (failureNotFound $ toString ref)
    (\object ->
        (fromJust . decode $ encode object :: GitObject) `shouldBe` object)

readHashWorkObject :: FilePath -> Ref -> Expectation
readHashWorkObject path sha1 = withRepo path (workObject sha1) >>= maybe
  (failureNotFound $ toString sha1)
  (\object -> hashWorkObject object `shouldBe` sha1)

failureNotFound :: String -> Expectation
failureNotFound string = expectationFailure $ string ++ " not found"
