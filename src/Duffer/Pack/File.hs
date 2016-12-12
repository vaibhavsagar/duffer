module Duffer.Pack.File where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Bool            (bool)
import Data.Tuple           (swap)
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack.Parser   (hashResolved, parseResolved, parsedIndex)
import Duffer.Pack.Entries  (PackDecompressed(..), PackDelta(..), PackEntry(..)
                            ,PackedObject(..), DeltaInstruction(..), Delta(..)
                            ,CombinedMap(..), RefIndex, OffsetMap, ObjectMap(..)
                            ,fullObject, toAssoc, emptyObjectMap
                            ,isResolved, insertObject)

applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source = B.concat . map (`applyInstruction` source)

substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset len bytestring = B.take len (B.drop offset bytestring)

applyInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
applyInstruction instruction = case instruction of
    (CopyInstruction offset len) -> substring offset len
    (InsertInstruction content)     -> const content

resolveDelta :: CombinedMap -> Int -> PackedObject
resolveDelta combinedMap index = case getOffsetMap combinedMap Map.! index of
    Resolved object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    UnResolved (OfsDelta o (PackDecompressed l (Delta _ _ instructions))) -> let
        -- Find base object type and source.
        PackedObject t _ source = resolveDelta combinedMap (index-o)
        -- Interpret the delta instructions with the provided source.
        resolvedDelta           = (`applyInstructions` instructions) <$> source
        -- The resulting ByteString can be parsed to yield an object.
        resultingHash           = hashResolved t resolvedDelta
        -- We now have an object of type t with a hash and a ByteString.
        in PackedObject t resultingHash (resolvedDelta {packLevel = l})
    UnResolved (RefDelta r (PackDecompressed l (Delta _ _ instructions))) -> let
        refIndex                = (Map.!) (getRefIndex combinedMap) r
        PackedObject t _ source = resolveDelta combinedMap refIndex
        -- Resolve the delta against this source.
        resolvedDelta           = (`applyInstructions` instructions) <$> source
        -- Compute the hash of this object.
        resultingHash           = hashResolved t resolvedDelta
        in PackedObject t resultingHash (resolvedDelta {packLevel = l})

resolveEntry :: CombinedMap -> Ref -> Maybe GitObject
resolveEntry combinedMap ref = case Map.lookup ref (getRefIndex combinedMap) of
    Just offset -> Just $ unpackObject $ resolveDelta combinedMap offset
    Nothing     -> Nothing

unpackObject :: PackedObject -> GitObject
unpackObject (PackedObject t _ content) = parseResolved t content

makeRefIndex :: B.ByteString -> RefIndex
makeRefIndex content = let
    index = parsedIndex content
    in Map.fromList $ map (swap . toAssoc) index

makeOffsetMap :: B.ByteString -> Map.Map Int Ref
makeOffsetMap content = let
    index = parsedIndex content
    in Map.fromList $ map toAssoc index

resolveAll' :: OffsetMap -> [GitObject]
resolveAll' =
    map unpackObject . Map.elems . getObjectMap . resolveIter emptyObjectMap

resolveIter :: ObjectMap -> OffsetMap -> ObjectMap
resolveIter objectMap offsetMap | Map.null offsetMap = objectMap
resolveIter objectMap offsetMap = let
    (objectMap', offsetMap') = separateResolved objectMap offsetMap
    resolve                  = resolveIfPossible objectMap'
    offsetMap''              = Map.mapWithKey resolve offsetMap'
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
    UnResolved (OfsDelta o' delta) | Map.member (o-o') oMap -> let
        base = oMap Map.! (o-o')
        in resolve base delta
    UnResolved (RefDelta r' delta) | Map.member r' oIndex -> let
        base = oMap Map.! (oIndex Map.! r')
        in resolve base delta
    _ -> entry
    where resolve (PackedObject t _ source) (PackDecompressed l (Delta _ _ is)) =
            let
                resolved = (`applyInstructions` is) <$> source
                r        = hashResolved t resolved
                in Resolved $ PackedObject t r (resolved {packLevel = l})
