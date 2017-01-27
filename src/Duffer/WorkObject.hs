{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Duffer.WorkObject where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set        as S

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor.Compose      (Compose(..))
import Data.Functor.Identity     (Identity(..))
import Duffer.Loose.Objects
import Duffer                    (readObject, writeObject)
import Duffer.WithRepo

type WorkObject = GitObjectGeneric Ref WorkTreeEntryMap

type WorkTreeEntryMap = Map.Map B.ByteString WorkTreeEntry

data WorkTreeEntry = WorkTreeEntry WorkObject EntryPermission

convert
    :: Applicative f
    => (a -> f c)
    -> (b -> f d)
    -> GitObjectGeneric a b
    -> f (GitObjectGeneric c d)
convert f g = \case
    Blob{..}   -> pure Blob{..}
    Tree{..}   -> Tree <$> g treeEntries
    Commit{..} -> Commit
        <$> f commitTreeRef
        <*> traverse f commitParentRefs
        <*> pure commitAuthor
        <*> pure commitCommitter
        <*> pure commitMessage
    Tag{..}    -> Tag
        <$> f tagObjectRef
        <*> pure tagObjectType
        <*> pure tagName
        <*> pure tagTagger
        <*> pure tagAnnotation

workObject :: Ref -> WithRepo (Maybe WorkObject)
workObject ref = runMaybeT $ MaybeT (readObject ref) >>=
    convert pure (MaybeT . workTreeEntries)

workTreeEntries :: S.Set TreeEntry -> WithRepo (Maybe WorkTreeEntryMap)
workTreeEntries entries = do
    let entriesL    = S.toList entries
    let filenames   = map entryName  entriesL
    let permissions = map entryPerms entriesL
    children <- getCompose $ traverse (Compose . workObject . entryRef) entriesL
    let wtEntries   = zipWith WorkTreeEntry <$> children <*> pure permissions
    return $ Map.fromList . zip filenames <$> wtEntries

hashWorkObject :: WorkObject -> Ref
hashWorkObject = hash . toGitObject

toGitObject :: WorkObject -> GitObject
toGitObject = runIdentity . convert pure (Identity . hashEntries)
    where hashEntries = Map.foldrWithKey
            (\k e -> S.insert $ makeTreeEntry k e) S.empty

makeTreeEntry :: B.ByteString -> WorkTreeEntry -> TreeEntry
makeTreeEntry entryName (WorkTreeEntry wo entryPerms) = let
    entryRef = hashWorkObject wo
    in TreeEntry{..}

writeWorkObject :: WorkObject -> WithRepo Ref
writeWorkObject wObject = writeObject =<< convert pure writeTreeEntries wObject

writeTreeEntries :: WorkTreeEntryMap -> WithRepo (S.Set TreeEntry)
writeTreeEntries = fmap S.fromList . traverse writeEntry . Map.toList
    where writeEntry (entryName, WorkTreeEntry wt entryPerms) = do
            entryRef <- writeWorkObject wt
            return TreeEntry{..}
