module Duffer.Pack where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map.Strict      as Map

import Control.Monad              (filterM)
import GHC.Int                    (Int64)
import System.IO.MMap             (mmapFileByteString)
import System.FilePath            ((</>), (-<.>), combine, takeExtension)
import System.Directory           (getDirectoryContents)

import Duffer.Loose.Objects (GitObject, Ref)
import Duffer.Pack.Entries
import Duffer.Pack.Parser
import Duffer.Pack.File
import Duffer.WithRepo

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

hasPacked :: Ref -> FilePath -> IO Bool
hasPacked ref indexPath = do
    content  <- B.readFile indexPath
    let refs =  parsedPackIndexRefs content
    return $ ref `elem` refs

readPackObject :: Ref -> WithRepo (Maybe GitObject)
readPackObject ref = do
    path <- ask
    liftIO $ readPacked ref path

readPacked :: Ref -> FilePath -> IO (Maybe GitObject)
readPacked ref path = do
    indices <- getPackIndices path
    matches <- filterM (hasPacked ref) indices
    case matches of
        []      -> return Nothing
        index:_ -> flip resolveEntry ref <$> combinedEntryMap index

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
    offsetMap    <- makeOffsetMap <$> B.readFile indexPath
    let filePath =  packFile indexPath
    contentEnd   <- B.length <$> B.readFile filePath
    let indices  =  Map.keys offsetMap
    let rangeMap =  Map.insert (contentEnd - 20) "" offsetMap
    entries      <- mapM (getPackRegion filePath rangeMap) indices
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

readPackRef :: FilePath -> WithRepo (Maybe Ref)
readPackRef refPath = do
    refsPath <- asks (</> "packed-refs")
    content  <- liftIO $ B.readFile refsPath
    let refs = parsedPackRefs content
    return $ Map.lookup (BU.fromString refPath) refs

resolvePackRef :: FilePath -> WithRepo (Maybe GitObject)
resolvePackRef refPath = do
    maybeRef <- readPackRef refPath
    case maybeRef of
        Nothing  -> return Nothing
        Just ref -> readPackObject ref
