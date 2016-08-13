module Duffer.Pack where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Attoparsec.ByteString (parseOnly)
import GHC.Int (Int64)
import System.IO.MMap (mmapFileByteString)
import System.FilePath ((-<.>))

import Duffer.Loose.Objects (GitObject)
import Duffer.Pack.Entries
import Duffer.Pack.Parser
import Duffer.Pack.File

packFile, packIndex :: FilePath -> FilePath
packFile  path = path -<.> "pack"
packIndex path = path -<.> "idx"

region :: Map.Map Int a -> Int -> Maybe (Int64, Int)
region offsetMap offset = let
    (Just nextOffset) = Map.lookupGT offset offsetMap
    len               = fst nextOffset - offset
    in Just (fromIntegral offset, len)

packFileRegion :: FilePath -> Maybe (Int64, Int) -> IO B.ByteString
packFileRegion = mmapFileByteString

getPackFileEntry :: FilePath -> Map.Map Int B.ByteString -> Int -> IO PackEntry
getPackFileEntry packFilePath rangeMap index = do
    content <- packFileRegion packFilePath (region rangeMap index)
    return $ either error id (parseOnly parsePackRegion content)

indexedEntryMap :: FilePath -> IO OffsetMap
indexedEntryMap indexPath = do
    indexContent <- B.readFile indexPath
    let offsetMap = makeOffsetMap indexContent
    let indices   = Map.keys offsetMap
    let filePath  = packFile indexPath
    contentEnd <- B.length <$> B.readFile filePath
    let rangeMap  = Map.insert (contentEnd - 20) "" offsetMap
    entries <- mapM (getPackFileEntry filePath rangeMap) indices
    return $ Map.fromAscList $ zip indices entries

combinedEntryMap :: FilePath -> IO CombinedMap
combinedEntryMap indexPath = do
    indexedMap <- indexedEntryMap indexPath
    refIndex   <- makeRefIndex <$> B.readFile indexPath
    return $ CombinedMap indexedMap refIndex

resolveAll :: FilePath -> IO [GitObject]
resolveAll indexPath = do
    combined <- combinedEntryMap indexPath
    let reconstitute = unpackObject . resolveDelta combined
    return $ map reconstitute $ Map.elems (getRefIndex combined)

resolveAll' :: FilePath -> IO [GitObject]
resolveAll' indexPath = do
    indexedMap <- indexedEntryMap indexPath
    let objectMap = resolveIter emptyObjectMap indexedMap
    return $ map unpackObject $ Map.elems $ getObjectMap objectMap
