module Duffer.Pack.File where

import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IntMap

import Prelude hiding       (concat, take, drop)

import Data.Bool            (bool)
import Data.ByteString      (ByteString, concat, take, drop)
import Data.Tuple           (swap)
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack.Parser   (hashResolved, parseResolved, parsedIndex)
import Duffer.Pack.Entries  (WCL(..), PackDelta(..), PackEntry(..)
                            ,PackedObject(..), DeltaInstruction(..), Delta(..)
                            ,CombinedMap(..), RefIndex, OffsetMap, ObjectMap(..)
                            ,toAssoc, emptyObjectMap, isResolved, insertObject)

applyInstructions :: ByteString -> [DeltaInstruction] -> ByteString
applyInstructions source = concat . map (`applyInstruction` source)

substring :: Int -> Int -> ByteString -> ByteString
substring offset len = take len . drop offset

applyInstruction :: DeltaInstruction -> ByteString -> ByteString
applyInstruction (CopyInstruction offset len) = substring offset len
applyInstruction (InsertInstruction content)  = const content

resolve :: PackedObject -> WCL Delta -> PackedObject
resolve (PackedObject t _ (WCL _ source)) (WCL l (Delta _ _ is)) = let
    resolved = applyInstructions source is
    r        = hashResolved t resolved
    in PackedObject t r (WCL l resolved)

resolveDelta :: CombinedMap -> Int -> PackedObject
resolveDelta combinedMap index = case getOffsetMap combinedMap IntMap.! index of
    Resolved object -> object
    UnResolved (OfsDelta o delta) -> let
        source = resolveDelta combinedMap $ index-o
        in resolve source delta
    UnResolved (RefDelta r delta) -> let
        source = resolveDelta combinedMap $ getRefIndex combinedMap Map.! r
        in resolve source delta

resolveEntry :: CombinedMap -> Ref -> Maybe GitObject
resolveEntry combinedMap ref = unpackObject . resolveDelta combinedMap <$>
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
    possiblyResolved         = resolveIfPossible objectMap'
    offsetMap''              = IntMap.mapWithKey possiblyResolved offsetMap'
    in bool
        (error "cannot progress")
        (resolveIter objectMap' offsetMap'')
        (IntMap.size offsetMap' < IntMap.size offsetMap)

separateResolved :: ObjectMap -> OffsetMap -> (ObjectMap, OffsetMap)
separateResolved objectMap offsetMap = let
    (objects, deltas) = IntMap.partition isResolved offsetMap
    objects'          = IntMap.map (\(Resolved o) -> o) objects
    objectMap'        = IntMap.foldrWithKey insertObject objectMap objects'
    in (objectMap', deltas)

resolveIfPossible :: ObjectMap -> Int -> PackEntry -> PackEntry
resolveIfPossible (ObjectMap oMap oIndex) o entry = case entry of
    UnResolved (OfsDelta o' delta) | Just base <- IntMap.lookup (o-o') oMap ->
        Resolved $ resolve base                 delta
    UnResolved (RefDelta r' delta) | Just offs <- Map.lookup r' oIndex      ->
        Resolved $ resolve (oMap IntMap.! offs) delta
    _ -> entry
