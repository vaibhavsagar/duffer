module Duffer.Pack.File where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Duffer.Pack.Parser   (hashResolved)
import Duffer.Pack.Entries

applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source = B.concat . map (`applyInstruction` source)

substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset length bytestring = B.take length (B.drop offset bytestring)

applyInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
applyInstruction instruction = case instruction of
    (CopyInstruction offset length) -> substring offset length
    (InsertInstruction content)     -> const content

resolveDelta :: OffsetMap -> Int -> PackEntry
resolveDelta indexedObjects index = case (Map.!) indexedObjects index of
    object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    PackedDelta (OfsDelta o (Delta _ _ instructions)) -> let
        -- Find base object type and source.
        PackedObject t _ source = resolveDelta indexedObjects (index-o)
        -- Interpret the delta instructions with the provided source.
        resolvedDelta           = applyInstructions source instructions
        -- The resulting ByteString can be parsed to yield an object.
        resultingHash           = hashResolved t resolvedDelta
        -- We now have an object of type t with a hash and a ByteString.
        in PackedObject t resultingHash resolvedDelta
    PackedDelta (RefDelta r (Delta _ _ instructions)) -> let
        -- The base object is behind this one in the packfile.
        previousPacked           = fst $ Map.split index indexedObjects
        -- Find the most recent commit, tree, blob, or tag.
        t                        = getCurrentType previousPacked
        -- Remove all entries before the first of type t.
        possibleBases            = getRestFrom previousPacked t
        -- Compute SHA1 references for each previous entry
        refMap                   = makeRefMap possibleBases
        -- Find the base object.
        PackedObject t' _ source = (Map.!) refMap r
        -- Resolve the delta against this source.
        resolvedDelta            = applyInstructions source instructions
        -- Compute the hash of this object.
        resultingHash            = hashResolved t' resolvedDelta
        in PackedObject t' resultingHash resolvedDelta

resolveDelta' :: CombinedMap -> Int -> PackEntry
resolveDelta' combinedMap index = case (Map.!) (getOffsetMap combinedMap) index of
    object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    PackedDelta (OfsDelta o (Delta _ _ instructions)) -> let
        -- Find base object type and source.
        PackedObject t _ source = resolveDelta' combinedMap (index-o)
        -- Interpret the delta instructions with the provided source.
        resolvedDelta           = applyInstructions source instructions
        -- The resulting ByteString can be parsed to yield an object.
        resultingHash           = hashResolved t resolvedDelta
        -- We now have an object of type t with a hash and a ByteString.
        in PackedObject t resultingHash resolvedDelta
    PackedDelta (RefDelta r (Delta _ _ instructions)) -> let
        refIndex                 = (Map.!) (getRefIndex combinedMap) r
        PackedObject t' _ source = resolveDelta' combinedMap refIndex
        -- Resolve the delta against this source.
        resolvedDelta            = applyInstructions source instructions
        -- Compute the hash of this object.
        resultingHash            = hashResolved t' resolvedDelta
        in PackedObject t' resultingHash resolvedDelta

getCurrentType :: OffsetMap -> PackObjectType
getCurrentType packMap = case snd (Map.findMax packMap) of
    PackedObject t _ _ -> t
    _                  -> getCurrentType (Map.deleteMax packMap)

getRestFrom :: OffsetMap -> PackObjectType -> OffsetMap
getRestFrom packMap t = case snd (Map.findMin packMap) of
    PackedObject t _ _ -> packMap
    _                  -> getRestFrom (Map.deleteMin packMap) t

makeRefMap :: OffsetMap -> RefMap
makeRefMap originalMap = loop originalMap Map.empty
    where loop indexedMap refsMap | Map.null indexedMap = refsMap
          loop indexedMap refsMap = case Map.findMin indexedMap of
            (_, object@(PackedObject _ r _))       -> let
                indexedMap'        = Map.deleteMin indexedMap
                refsMap'           = Map.insert r object refsMap
                in loop indexedMap' refsMap'
            (offset, PackedDelta OfsDelta{}) -> let
                resolved           = resolveDelta originalMap offset
                PackedObject _ r _ = resolved
                indexedMap'        = Map.deleteMin indexedMap
                refsMap'           = Map.insert r resolved refsMap
                in loop indexedMap' refsMap'
            (_, PackedDelta (RefDelta r (Delta _ _ instructions))) -> let
                PackedObject t _ s = (Map.!) refsMap r
                resolvedDelta      = applyInstructions s instructions
                objectRef          = hashResolved t resolvedDelta
                entry              = PackedObject t objectRef resolvedDelta
                indexedMap'        = Map.deleteMin indexedMap
                refsMap'           = Map.insert objectRef entry refsMap
                in loop indexedMap' refsMap'
