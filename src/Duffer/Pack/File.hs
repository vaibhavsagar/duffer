module Duffer.Pack.File where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Either          (isLeft)
import Data.Tuple           (swap)
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack.Parser   (hashResolved, parseResolved, parsedIndex)
import Duffer.Pack.Entries

applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source = B.concat . map (`applyInstruction` source)

substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset length bytestring = B.take length (B.drop offset bytestring)

applyInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
applyInstruction instruction = case instruction of
    (CopyInstruction offset length) -> substring offset length
    (InsertInstruction content)     -> const content

resolveDelta :: CombinedMap -> Int -> PackedObject
resolveDelta combinedMap index = case (Map.!) (getOffsetMap combinedMap) index of
    Left object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    Right (OfsDelta o (Delta _ _ instructions)) -> let
        -- Find base object type and source.
        PackedObject t _ source = resolveDelta combinedMap (index-o)
        -- Interpret the delta instructions with the provided source.
        resolvedDelta           = applyInstructions source instructions
        -- The resulting ByteString can be parsed to yield an object.
        resultingHash           = hashResolved t resolvedDelta
        -- We now have an object of type t with a hash and a ByteString.
        in PackedObject t resultingHash resolvedDelta
    Right (RefDelta r (Delta _ _ instructions)) -> let
        refIndex                 = (Map.!) (getRefIndex combinedMap) r
        PackedObject t' _ source = resolveDelta combinedMap refIndex
        -- Resolve the delta against this source.
        resolvedDelta            = applyInstructions source instructions
        -- Compute the hash of this object.
        resultingHash            = hashResolved t' resolvedDelta
        in PackedObject t' resultingHash resolvedDelta

unpackObject :: PackedObject -> GitObject
unpackObject (PackedObject t _ content) = parseResolved t content

makeRefIndex :: B.ByteString -> Map.Map Ref Int
makeRefIndex content = let
    index = parsedIndex content
    in Map.fromList $ map (swap . toAssoc) index

makeOffsetMap :: B.ByteString -> Map.Map Int Ref
makeOffsetMap content = let
    index = parsedIndex content
    in Map.fromList $ map toAssoc index

separateResolved :: ObjectMap -> OffsetMap -> (ObjectMap, OffsetMap)
separateResolved objectMap offsetMap = let
    (objects, deltas) = Map.partition isLeft offsetMap
    objects'          = Map.map (\(Left o) -> o) objects
    objectMap'      = Map.foldrWithKey insertObject objectMap objects'
    in (objectMap', deltas)

resolveIfPossible :: ObjectMap -> Int -> PackEntry -> PackEntry
resolveIfPossible objectMap offset entry = case entry of
    Right (OfsDelta o (Delta _ _ instructions))
        | (offset-o) `elem` Map.elems (getObjectIndex objectMap) -> let
            PackedObject t _ source = getObjectMap objectMap Map.! (offset-o)
            resolved = applyInstructions source instructions
            r        = hashResolved t resolved
            in Left $ PackedObject t r resolved
    Right (RefDelta r (Delta _ _ instructions))
        | Map.member r (getObjectIndex objectMap) -> let
            index                   = getObjectIndex objectMap Map.! r
            PackedObject t _ source = getObjectMap   objectMap Map.! index
            resolved = applyInstructions source instructions
            r'       = hashResolved t resolved
            in Left $ PackedObject t r' resolved
    _ -> entry

resolveIter :: ObjectMap -> OffsetMap -> ObjectMap
resolveIter objectMap offsetMap | Map.null offsetMap = objectMap
resolveIter objectMap offsetMap = let
    (objectMap', offsetMap') = separateResolved objectMap offsetMap
    offsetMap''                = Map.mapWithKey (resolveIfPossible objectMap') offsetMap'
    in if Map.size offsetMap' < Map.size offsetMap
        then resolveIter objectMap' offsetMap''
        else error "cannot progress"
