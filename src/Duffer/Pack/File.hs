module Duffer.Pack.File where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Tuple           (swap)
import Duffer.Loose.Objects (Ref)
import Duffer.Pack.Parser   (hashResolved, parsedIndex)
import Duffer.Pack.Entries

applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source = B.concat . map (`applyInstruction` source)

substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset length bytestring = B.take length (B.drop offset bytestring)

applyInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
applyInstruction instruction = case instruction of
    (CopyInstruction offset length) -> substring offset length
    (InsertInstruction content)     -> const content

resolveDelta :: CombinedMap -> Int -> PackEntry
resolveDelta combinedMap index = case (Map.!) (getOffsetMap combinedMap) index of
    object@(PackedObject t _ _)
        -- If we find a commit, tree, blob, or tag, our work is done.
        | fullObject t -> object
        | otherwise    -> error "PackedObject cannot contain deltas"
    -- An OfsDelta needs to be resolved against a base object
    PackedDelta (OfsDelta o (Delta _ _ instructions)) -> let
        -- Find base object type and source.
        PackedObject t _ source = resolveDelta combinedMap (index-o)
        -- Interpret the delta instructions with the provided source.
        resolvedDelta           = applyInstructions source instructions
        -- The resulting ByteString can be parsed to yield an object.
        resultingHash           = hashResolved t resolvedDelta
        -- We now have an object of type t with a hash and a ByteString.
        in PackedObject t resultingHash resolvedDelta
    PackedDelta (RefDelta r (Delta _ _ instructions)) -> let
        refIndex                 = (Map.!) (getRefIndex combinedMap) r
        PackedObject t' _ source = resolveDelta combinedMap refIndex
        -- Resolve the delta against this source.
        resolvedDelta            = applyInstructions source instructions
        -- Compute the hash of this object.
        resultingHash            = hashResolved t' resolvedDelta
        in PackedObject t' resultingHash resolvedDelta

makeRefIndex :: B.ByteString -> Map.Map Ref Int
makeRefIndex content = let
    index = parsedIndex content
    in Map.fromList $ map (swap . toAssoc) index

makeOffsetMap :: B.ByteString -> Map.Map Int Ref
makeOffsetMap content = let
    index = parsedIndex content
    in Map.fromList $ map toAssoc index
