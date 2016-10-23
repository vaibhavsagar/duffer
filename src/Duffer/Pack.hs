module Duffer.Pack where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import GHC.Int          (Int64)
import System.IO.MMap   (mmapFileByteString)
import System.FilePath  ((-<.>), combine, takeExtension)
import System.Directory (getDirectoryContents)

import Duffer.Loose.Objects (GitObject)
import Duffer.Pack.Entries
import Duffer.Pack.Parser
import Duffer.Pack.File

packFile, packIndex :: FilePath -> FilePath
packFile  = (-<.> "pack")
packIndex = (-<.> "idx")

region :: Map.Map Int a -> Int -> Maybe (Int64, Int)
region offsetMap offset = let
    (Just nextOffset) = Map.lookupGT offset offsetMap
    len               = fst nextOffset - offset
    in Just (fromIntegral offset, len)

getPackIndices :: FilePath -> IO [FilePath]
getPackIndices path = let packFilePath = path ++ "/objects/pack" in
    map (combine packFilePath) . filter (\f -> takeExtension f == ".idx") <$>
    getDirectoryContents packFilePath

getPackFileEntry :: FilePath -> Map.Map Int B.ByteString -> Int -> IO PackEntry
getPackFileEntry packFilePath rangeMap index =
    parsedPackRegion <$> getPackRegion packFilePath rangeMap index

getPackRegion :: FilePath -> Map.Map Int B.ByteString -> Int -> IO B.ByteString
getPackRegion packFilePath rangeMap index =
    packFileRegion packFilePath (region rangeMap index)

packFileRegion :: FilePath -> Maybe (Int64, Int) -> IO B.ByteString
packFileRegion = mmapFileByteString

indexedEntryMap :: FilePath -> IO OffsetMap
indexedEntryMap = fmap (Map.map parsedPackRegion) . indexedByteStringMap

indexedByteStringMap :: FilePath -> IO (Map.Map Int B.ByteString)
indexedByteStringMap indexPath = do
    offsetMap     <- makeOffsetMap <$> B.readFile indexPath
    let filePath  =  packFile indexPath
    contentEnd    <- B.length <$> B.readFile filePath
    let indices   =  Map.keys offsetMap
    let rangeMap  =  Map.insert (contentEnd - 20) "" offsetMap
    entries       <- mapM (getPackRegion filePath rangeMap) indices
    return $ Map.fromAscList $ zip indices entries

combinedEntryMap :: FilePath -> IO CombinedMap
combinedEntryMap indexPath = CombinedMap
    <$> indexedEntryMap              indexPath
    <*> (makeRefIndex <$> B.readFile indexPath)

resolveAll :: FilePath -> IO [GitObject]
resolveAll indexPath = do
    combined         <- combinedEntryMap indexPath
    let reconstitute =  unpackObject . resolveDelta combined
    return $ map reconstitute $ Map.elems (getRefIndex combined)
