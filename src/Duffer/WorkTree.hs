{-# LANGUAGE OverloadedStrings #-}

module Duffer.WorkTree where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set as S
import Duffer.Loose.Objects
import Data.Maybe (fromJust)
import Duffer (readObject)
import Duffer.WithRepo

newtype WorkTreeBlob = WorkTreeBlob {wtBlobContent :: B.ByteString}
    deriving (Show)

data WorkTreeEntry
   = WorkTreeEntry (Either WorkTreeBlob WorkTree) EntryPermission
    deriving (Show)

newtype WorkTree = WorkTree (Map.Map B.ByteString WorkTreeEntry)
    deriving (Show)

workTree :: Ref -> WithRepo (Either WorkTreeBlob WorkTree)
workTree ref = do
    obj <- fromJust <$> readObject ref
    case obj of
        Blob content -> return $ Left (WorkTreeBlob content)
        Tree entries -> Right <$> workTreeEntries (Tree entries)

workTreeEntries :: GitObject -> WithRepo WorkTree
workTreeEntries (Tree entries) = do
    let entriesList = S.toList entries
    let filenames   = map entryName entriesList
    let permissions = map entryPerms entriesList
    children        <- mapM (workTree . entryRef) entriesList
    let workTreeEntryValues = zipWith WorkTreeEntry children permissions
    return $ WorkTree $ Map.fromList $ zip filenames workTreeEntryValues
