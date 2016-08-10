module Duffer.Pack.Entries where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Bits (Bits, shiftR, (.&.))

import Duffer.Loose.Objects (Ref)

data PackIndexEntry
    = PackIndexEntry Int Ref
    deriving (Show)

data PackObjectType
    = UnusedPackObjectType0
    | CommitObject
    | TreeObject
    | BlobObject
    | TagObject
    | UnusedPackObjectType5
    | OfsDeltaObject
    | RefDeltaObject
    deriving (Enum, Eq, Show)

data PackDelta
    = OfsDelta Int Delta
    | RefDelta Ref Delta
    deriving (Show)

data PackEntry
    = PackedObject PackObjectType Ref B.ByteString
    | PackedDelta  PackDelta
    deriving (Show)

data DeltaInstruction
    = CopyInstruction   Int Int
    | InsertInstruction B.ByteString
    deriving (Show)

data Delta = Delta Int Int [DeltaInstruction] deriving (Show)

data CombinedMap
    = CombinedMap
        { getOffsetMap :: OffsetMap
        , getRefIndex  :: Map.Map Ref Int
        }

type OffsetMap = Map.Map Int PackEntry
type RefMap    = Map.Map Ref PackEntry

fullObject :: PackObjectType -> Bool
fullObject t = t `elem` [CommitObject, TreeObject, BlobObject, TagObject]

packObjectType :: (Bits t, Integral t) => t -> PackObjectType
packObjectType header = toEnum . fromIntegral $ (header `shiftR` 4) .&. 7

toAssoc :: PackIndexEntry -> (Int, Ref)
toAssoc (PackIndexEntry o r) = (o, r)
