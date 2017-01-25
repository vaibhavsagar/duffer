{-# LANGUAGE LambdaCase #-}

module Duffer.WorkTree where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set as S
import Duffer.Loose.Objects
import Duffer (readObject)
import Duffer.WithRepo

data WorkObject
    = WorkBlob {wBlobContent :: B.ByteString}
    | WorkTree {wTreeEntries :: Map.Map B.ByteString WorkTreeEntry}
    deriving (Show)

data WorkTreeEntry = WorkTreeEntry WorkObject EntryPermission
    deriving (Show)

workObject :: Ref -> WithRepo (Maybe WorkObject)
workObject ref = readObject ref >>= \case
        Nothing             -> return Nothing
        Just (Blob content) -> return . Just $ WorkBlob content
        Just (Tree entries) -> workTreeEntries entries
        -- Haven't figured out what to do in this case yet.
        Just _              -> return Nothing

workTreeEntries :: S.Set TreeEntry -> WithRepo (Maybe WorkObject)
workTreeEntries entries = do
    let entriesList = S.toList entries
    let filenames   = map entryName  entriesList
    let permissions = map entryPerms entriesList
    children <- sequence <$> traverse (workObject . entryRef) entriesList
    let wtEntries   = zipWith WorkTreeEntry <$> children <*> pure permissions
    return $ WorkTree . Map.fromList . zip filenames <$> wtEntries
