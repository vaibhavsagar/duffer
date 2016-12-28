{-# LANGUAGE LambdaCase #-}

module Duffer.Pack.File where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Bool            (bool)
import Data.Tuple           (swap)
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack.Parser   (hashResolved, parseResolved, parsedIndex)
import Duffer.Pack.Entries  (WCL(..), PackDelta(..), PackEntry(..)
                            ,PackedObject(..), DeltaInstruction(..), Delta(..)
                            ,CombinedMap(..), RefIndex, OffsetMap, ObjectMap(..)
                            ,fullObject, toAssoc, emptyObjectMap ,isResolved
                            ,insertObject)

applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source = B.concat . map (`applyInstruction` source)

substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset len bytestring = B.take len (B.drop offset bytestring)

applyInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
applyInstruction = \case
    (CopyInstruction offset len) -> substring offset len
    (InsertInstruction content)  -> const content

resolve :: PackedObject -> WCL Delta -> PackedObject
resolve (PackedObject t _ source) (WCL l (Delta _ _ is)) = let
    resolved = (`applyInstructions` is) <$> source
    r        = hashResolved t resolved
    in PackedObject t r (resolved {wclLevel = l})

resolveDelta :: CombinedMap -> Int -> PackedObject
resolveDelta combinedMap index = case getOffsetMap combinedMap Map.! index of
    Resolved object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    UnResolved (OfsDelta o delta) -> let
        source = resolveDelta combinedMap (index-o)
        in resolve source delta
    UnResolved (RefDelta r delta) -> let
        refIndex = getRefIndex combinedMap Map.! r
        source   = resolveDelta combinedMap refIndex
        in resolve source delta

resolveEntry :: CombinedMap -> Ref -> Maybe GitObject
resolveEntry combinedMap ref = unpackObject . resolveDelta combinedMap <$>
    Map.lookup ref (getRefIndex combinedMap)

unpackObject :: PackedObject -> GitObject
unpackObject (PackedObject t _ content) = parseResolved t content

makeRefIndex :: B.ByteString -> RefIndex
makeRefIndex = Map.fromList . map (swap . toAssoc) . parsedIndex

makeOffsetMap :: B.ByteString -> Map.Map Int Ref
makeOffsetMap = Map.fromList . map toAssoc . parsedIndex

resolveAll' :: OffsetMap -> [GitObject]
resolveAll' =
    map unpackObject . Map.elems . getObjectMap . resolveIter emptyObjectMap

resolveIter :: ObjectMap -> OffsetMap -> ObjectMap
resolveIter objectMap offsetMap | Map.null offsetMap = objectMap
resolveIter objectMap offsetMap = let
    (objectMap', offsetMap') = separateResolved objectMap offsetMap
    possiblyResolved         = resolveIfPossible objectMap'
    offsetMap''              = Map.mapWithKey possiblyResolved offsetMap'
    in bool
        (error "cannot progress")
        (resolveIter objectMap' offsetMap'')
        (Map.size offsetMap' < Map.size offsetMap)

separateResolved :: ObjectMap -> OffsetMap -> (ObjectMap, OffsetMap)
separateResolved objectMap offsetMap = let
    (objects, deltas) = Map.partition isResolved offsetMap
    objects'          = Map.map (\(Resolved o) -> o) objects
    objectMap'        = Map.foldrWithKey insertObject objectMap objects'
    in (objectMap', deltas)

resolveIfPossible :: ObjectMap -> Int -> PackEntry -> PackEntry
resolveIfPossible (ObjectMap oMap oIndex) o entry = case entry of
    UnResolved (OfsDelta o' delta) | Just base <- Map.lookup (o-o') oMap ->
        Resolved $ resolve base              delta
    UnResolved (RefDelta r' delta) | Just offs <- Map.lookup r' oIndex   ->
        Resolved $ resolve (oMap Map.! offs) delta
    _ -> entry
