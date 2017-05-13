{-# LANGUAGE LambdaCase #-}

module Duffer.Pack (module Duffer.Pack) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map.Strict      as Map
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Set             as Set

import Control.Monad          (filterM, (<$!>))
import Data.Bool              (bool)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.IntMap.Strict     (IntMap)
import Data.Maybe             (fromJust)
import Foreign.C.String       (CStringLen)
import Foreign.Ptr            (Ptr, castPtr)
import GHC.Int                (Int64)
import System.Directory       (getDirectoryContents, doesFileExist)
import System.FilePath        ((</>), (-<.>), takeExtension)
import System.IO.MMap         (mmapWithFilePtr, Mode(..))

import Duffer.Loose.Objects (GitObject, Ref)
import Duffer.Pack.Entries  (CombinedMap(..), OffsetMap, getRefIndex)
import Duffer.Pack.Parser   (parsedPackIndexRefs, parsedPackRegion
                            ,parsedPackRefs)
import Duffer.Pack.File     (resolveEntry, resolveDelta, makeRefIndex
                            ,makeOffsetMap, unpackObject)
import Duffer.WithRepo      (WithRepo, ask, asks, liftIO, localObjects)

packFile, packIndex :: FilePath -> FilePath
packFile  = (-<.> "pack")
packIndex = (-<.> "idx")

region :: IntMap a -> Int -> Maybe (Int64, Int)
region offsetMap offset = let
    nextOffset = fromJust $ IntMap.lookupGT offset offsetMap
    in Just (fromIntegral offset, fst nextOffset - offset)

getPackIndices :: FilePath -> IO [FilePath]
getPackIndices path = let packFilePath = path </> "pack" in
    map (packFilePath </>) . filter ((==) ".idx" . takeExtension) <$>
    getDirectoryContents packFilePath

getPackObjectRefs :: WithRepo (Set.Set Ref)
getPackObjectRefs = Set.fromList . concatMap parsedPackIndexRefs <$>
    (asks getPackIndices >>= liftIO . (=<<) (traverse B.readFile))

hasPacked :: Ref -> FilePath -> IO Bool
hasPacked ref = fmap (elem ref . parsedPackIndexRefs) . B.readFile

hasPackObject :: Ref -> WithRepo Bool
hasPackObject = localObjects . hasPackObject'

hasPackObject' :: Ref -> WithRepo Bool
hasPackObject' ref = do
    paths <- asks getPackIndices
    or <$> liftIO (traverse (hasPacked ref) =<< paths)

readPackObject :: Ref -> WithRepo (Maybe GitObject)
readPackObject = localObjects . readPackObject'

readPackObject' :: Ref -> WithRepo (Maybe GitObject)
readPackObject' ref = (>>=) ask (liftIO . readPacked ref)

readPacked :: Ref -> FilePath -> IO (Maybe GitObject)
readPacked ref path = getPackIndices path >>= filterM (hasPacked ref) >>= \case
    []      -> return Nothing
    index:_ -> flip resolveEntry ref <$> combinedEntryMap index

getPackPtr :: FilePath -> IntMap b -> ((Ptr (), Int) -> IO a) -> Int -> IO a
getPackPtr packFilePath rangeMap action offset =
    mmapWithFilePtr packFilePath ReadOnly (region rangeMap offset) action

indexedEntryMap :: FilePath -> IO OffsetMap
indexedEntryMap indexPath = do
    offsetMap    <- makeOffsetMap <$> B.readFile indexPath
    let filePath =  packFile indexPath
    contentEnd   <- B.length <$> B.readFile filePath
    let indices  =  IntMap.keys offsetMap
    let rangeMap =  IntMap.insert (contentEnd - 20) "" offsetMap
    entries      <- traverse (getPackPtr filePath rangeMap parse) indices
    return . IntMap.fromAscList $ zip indices entries
    where parse (ptr, size) = let cStr = (castPtr ptr, size) :: CStringLen in
            parsedPackRegion <$!> unsafePackCStringLen cStr

combinedEntryMap :: FilePath -> IO CombinedMap
combinedEntryMap indexPath = CombinedMap
    <$> indexedEntryMap               indexPath
    <*> fmap makeRefIndex (B.readFile indexPath)

resolveAll :: CombinedMap -> [GitObject]
resolveAll = go <*> (Map.elems . getRefIndex)
    where go _Map []     = []
          go cMap (x:xs) = let (o, cMap') = resolveDelta cMap x
            in unpackObject o : go cMap' xs

readPackRef :: FilePath -> WithRepo (Maybe Ref)
readPackRef refPath = asks (</> "packed-refs") >>= \refsPath ->
    liftIO (doesFileExist refsPath) >>= bool
        (return Nothing)
        (Map.lookup (BU.fromString refPath) . parsedPackRefs <$>
            liftIO (B.readFile refsPath))
