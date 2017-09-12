module Duffer.Pack.File (module Duffer.Pack.File) where

import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IntMap

import Prelude hiding       (concat, take, drop)

import Data.Bool            (bool)
import Data.ByteString      (ByteString, concat, take, drop)
import Data.Tuple           (swap)
import Numeric.Natural      (Natural)

import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack.Parser   (hashResolved, parseResolved, parsedIndex)
import Duffer.Pack.Entries  (WCL(..), PackDelta(..), PackEntry(..)
                            ,PackedObject(..), DeltaInstruction(..), Delta(..)
                            ,CombinedMap(..), RefIndex, OffsetMap, ObjectMap(..)
                            ,toAssoc, emptyObjectMap, isResolved, insertObject)

applyInstructions :: ByteString -> [DeltaInstruction] -> ByteString
applyInstructions source = concat . map (`applyInstruction` source)

substring :: Natural -> Natural -> ByteString -> ByteString
substring offset len = take (fromIntegral len) . drop (fromIntegral offset)

applyInstruction :: DeltaInstruction -> ByteString -> ByteString
applyInstruction (CopyInstruction offset len) = substring offset len
applyInstruction (InsertInstruction content)  = const content

resolve :: PackedObject -> WCL Delta -> PackedObject
resolve (PackedObject t _ (WCL _ source)) (WCL l (Delta _ _ is)) = let
    resolved = applyInstructions source is
    r        = hashResolved t resolved
    in PackedObject t r (WCL l resolved)

resolveDelta :: CombinedMap -> Natural -> (PackedObject, CombinedMap)
resolveDelta combinedMap i = case getOffsetMap combinedMap IntMap.! index of
    Resolved   object     -> (object, combinedMap)
    UnResolved unresolved -> let
        (delta, index') = case unresolved of
            OfsDelta o d -> (d, index - fromIntegral o)
            RefDelta r d -> (d, getRefIndex combinedMap Map.! r)
        (source, combinedMap') = resolveDelta combinedMap $ fromIntegral index'
        resolved = resolve source delta
        in (resolved, insertOffsetMap index (Resolved resolved) combinedMap')
    where
        insertOffsetMap key value cMap = cMap
            {getOffsetMap = IntMap.insert key value $ getOffsetMap cMap}
        index = fromIntegral i


resolveEntry :: CombinedMap -> Ref -> Maybe GitObject
resolveEntry combinedMap ref =
    unpackObject . fst . resolveDelta combinedMap . fromIntegral <$>
    Map.lookup ref (getRefIndex combinedMap)

unpackObject :: PackedObject -> GitObject
unpackObject (PackedObject t _ content) = parseResolved t $ wclContent content

makeRefIndex :: ByteString -> RefIndex
makeRefIndex = Map.fromList . map (swap . toAssoc) . parsedIndex

makeOffsetMap :: ByteString -> IntMap.IntMap Ref
makeOffsetMap = IntMap.fromList . map toAssoc . parsedIndex

resolveAll' :: OffsetMap -> [GitObject]
resolveAll' =
    map unpackObject . IntMap.elems . getObjectMap . resolveIter emptyObjectMap

resolveIter :: ObjectMap -> OffsetMap -> ObjectMap
resolveIter objectMap offsetMap | IntMap.null offsetMap = objectMap
resolveIter objectMap offsetMap = let
    (objectMap', offsetMap') = separateResolved objectMap offsetMap
    in bool
        (error "cannot progress")
        (resolveIter objectMap' $
            IntMap.mapWithKey (resolveIfPossible objectMap') offsetMap')
        (IntMap.size offsetMap' < IntMap.size offsetMap)

separateResolved :: ObjectMap -> OffsetMap -> (ObjectMap, OffsetMap)
separateResolved objectMap offsetMap = let
    (objects, deltas) = IntMap.partition isResolved offsetMap
    objects'          = IntMap.map resolved objects
    objectMap'        = IntMap.foldrWithKey insertObject objectMap objects'
    in (objectMap', deltas)
    where resolved (Resolved o)   = o
          resolved (UnResolved _) = error "only works with resolved"

resolveIfPossible :: ObjectMap -> Int -> PackEntry -> PackEntry
resolveIfPossible (ObjectMap oMap oIndex) o entry = case entry of
    UnResolved (OfsDelta o' delta)
        | Just base <- IntMap.lookup (o - fromIntegral o') oMap ->
            Resolved $ resolve base                 delta
    UnResolved (RefDelta r' delta)
        | Just offs <- Map.lookup r' oIndex                     ->
            Resolved $ resolve (oMap IntMap.! offs) delta
    _ -> entry
