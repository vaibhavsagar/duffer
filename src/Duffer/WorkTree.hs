{-# LANGUAGE LambdaCase #-}

module Duffer.WorkTree where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set as S
import Duffer.Loose.Objects
import Duffer (readObject)
import Duffer.WithRepo

newtype WorkTreeBlob = WorkTreeBlob {wtBlobContent :: B.ByteString}
    deriving (Show)

data WorkTreeEntry
   = WorkTreeEntry (Either WorkTreeBlob WorkTree) EntryPermission
    deriving (Show)

newtype WorkTree = WorkTree (Map.Map B.ByteString WorkTreeEntry)
    deriving (Show)


workTree :: Ref -> WithRepo (Maybe (Either WorkTreeBlob WorkTree))
workTree ref = readObject ref >>= \case
        Nothing             -> return Nothing
        Just (Blob content) -> return . Just $ Left (WorkTreeBlob content)
        Just (Tree entries) -> workTreeEntries entries >>= \case
            Just e  -> return . Just $ Right e
            Nothing -> return Nothing
        Just _              -> error "Commit or Tag found."

workTreeEntries :: S.Set TreeEntry -> WithRepo (Maybe WorkTree)
workTreeEntries entries = do
    let entriesList = S.toList entries
    let filenames = map entryName entriesList
    let permissions = map entryPerms entriesList
    children <- mapM (workTree . entryRef) entriesList
    let childrenS = sequence children
    let wtEntries = zipWith WorkTreeEntry <$> childrenS <*> pure permissions
    return $ WorkTree . Map.fromList . zip filenames <$> wtEntries
