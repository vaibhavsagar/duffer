{-# LANGUAGE LambdaCase #-}

module Duffer.Pack where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set

import Data.Bool                  (bool)
import Control.Monad              (filterM)
import GHC.Int                    (Int64)
import System.IO.MMap             (mmapFileByteString)
import System.FilePath            ((</>), (-<.>), combine, takeExtension)
import System.Directory           (getDirectoryContents, doesFileExist)

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
getPackIndices path = let packFilePath = path </> "pack" in
    map (combine packFilePath) . filter ((==) ".idx" . takeExtension) <$>
    getDirectoryContents packFilePath

getPackObjectRefs :: WithRepo (Set.Set Ref)
getPackObjectRefs = do
    path <- ask
    indices <- liftIO (mapM B.readFile =<< getPackIndices path)
    return $ Set.fromList $ concatMap parsedPackIndexRefs indices

hasPacked :: Ref -> FilePath -> IO Bool
hasPacked ref indexPath =
    (elem ref . parsedPackIndexRefs) <$> B.readFile indexPath

hasPackObject :: Ref -> WithRepo Bool
hasPackObject = localObjects . hasPackObject'

hasPackObject' :: Ref -> WithRepo Bool
hasPackObject' ref = do
    paths <- asks getPackIndices
    or <$> liftIO (mapM (hasPacked ref) =<< paths)

readPackObject :: Ref -> WithRepo (Maybe GitObject)
readPackObject = localObjects . readPackObject'

readPackObject' :: Ref -> WithRepo (Maybe GitObject)
readPackObject' ref = liftIO . readPacked ref =<< ask

readPacked :: Ref -> FilePath -> IO (Maybe GitObject)
readPacked ref path =
    (filterM (hasPacked ref) =<< getPackIndices path) >>= \case
        []      -> return Nothing
        index:_ -> flip resolveEntry ref <$> combinedEntryMap index

getPackFileEntry :: FilePath -> Map.Map Int B.ByteString -> Int -> IO PackEntry
getPackFileEntry packFilePath rangeMap index =
    parsedPackRegion <$> getPackRegion packFilePath rangeMap index

getPackRegion :: FilePath -> Map.Map Int B.ByteString -> Int -> IO B.ByteString
getPackRegion packFilePath rangeMap =
    packFileRegion packFilePath . region rangeMap

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
    <$> indexedEntryMap               indexPath
    <*> fmap makeRefIndex (B.readFile indexPath)

resolveAll :: FilePath -> IO [GitObject]
resolveAll indexPath = do
    combined         <- combinedEntryMap indexPath
    let reconstitute =  unpackObject . resolveDelta combined
    return $ map reconstitute $ Map.elems (getRefIndex combined)

readPackRef :: FilePath -> WithRepo (Maybe Ref)
readPackRef refPath = do
    refsPath <- asks (</> "packed-refs")
    liftIO (doesFileExist refsPath) >>= bool
        (return Nothing)
        (Map.lookup (BU.fromString refPath) . parsedPackRefs <$>
            liftIO (B.readFile refsPath))

resolvePackRef :: FilePath -> WithRepo (Maybe GitObject)
resolvePackRef refPath = do
    maybeRef <- readPackRef refPath
    case maybeRef of
        Nothing  -> return Nothing
        Just ref -> readPackObject ref
