{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Duffer.WorkObject where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set        as S (Set, toList, insert, empty)
import Duffer.Loose.Objects
import Duffer (readObject)
import Duffer.WithRepo

type WorkObject = GitObjectGeneric Ref (Map.Map B.ByteString WorkTreeEntry)

data WorkTreeEntry = WorkTreeEntry WorkObject EntryPermission

workObject :: Ref -> WithRepo (Maybe WorkObject)
workObject ref = readObject ref >>= \case
        Nothing  -> return Nothing
        Just obj -> case obj of
            Tree entries -> workTreeEntries entries
            Commit{..}   -> return . Just $ Commit{..}
            Blob{..}     -> return . Just $ Blob{..}
            Tag{..}      -> return . Just $ Tag{..}


workTreeEntries :: S.Set TreeEntry -> WithRepo (Maybe WorkObject)
workTreeEntries entries = do
    let entriesList = S.toList entries
    let filenames   = map entryName  entriesList
    let permissions = map entryPerms entriesList
    children <- sequence <$> traverse (workObject . entryRef) entriesList
    let wtEntries   = zipWith WorkTreeEntry <$> children <*> pure permissions
    return $ Tree . Map.fromList . zip filenames <$> wtEntries

hashWorkObject :: WorkObject -> Ref
hashWorkObject = hash . toGitObject

toGitObject :: WorkObject -> GitObject
toGitObject = \case
    Commit{..} -> Commit{..}
    Tag{..}    -> Tag{..}
    Blob{..}   -> Blob{..}
    Tree{..}   -> let
        entries = Map.foldrWithKey
            (\k e -> S.insert $ makeTreeEntry k e) S.empty treeEntries
        in Tree entries

makeTreeEntry :: B.ByteString -> WorkTreeEntry -> TreeEntry
makeTreeEntry entryName (WorkTreeEntry wo entryPerms) = let
    entryRef = hashWorkObject wo
    in TreeEntry{..}
