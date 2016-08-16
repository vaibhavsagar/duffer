module Duffer.Pack.Entries where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Bits
import Data.Word (Word32)

import Duffer.Loose.Objects (Ref)

data PackIndexEntry
    = PackIndexEntry Int Ref Word32
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
    deriving (Show, Eq)

data PackedObject =
    PackedObject PackObjectType Ref B.ByteString
    deriving (Show, Eq)

type PackEntry = Either PackedObject PackDelta

data DeltaInstruction
    = CopyInstruction   Int Int
    | InsertInstruction B.ByteString
    deriving (Show, Eq)

data Delta = Delta Int Int [DeltaInstruction] deriving (Show, Eq)

data CombinedMap
    = CombinedMap
        { getOffsetMap :: OffsetMap
        , getRefIndex  :: RefIndex
        }
        deriving (Show)

data ObjectMap
    = ObjectMap
    { getObjectMap   :: Map.Map Int PackedObject
    , getObjectIndex :: RefIndex
    }

type OffsetMap = Map.Map Int PackEntry
type RefMap    = Map.Map Ref PackEntry
type RefIndex  = Map.Map Ref Int

fullObject :: PackObjectType -> Bool
fullObject t = t `elem` [CommitObject, TreeObject, BlobObject, TagObject]

packObjectType :: (Bits t, Integral t) => t -> PackObjectType
packObjectType header = toEnum . fromIntegral $ (header `shiftR` 4) .&. 7

toAssoc :: PackIndexEntry -> (Int, Ref)
toAssoc (PackIndexEntry o r _) = (o, r)

emptyCombinedMap :: CombinedMap
emptyCombinedMap = CombinedMap Map.empty Map.empty

emptyObjectMap :: ObjectMap
emptyObjectMap = ObjectMap Map.empty Map.empty

insertObject :: Int -> PackedObject -> ObjectMap -> ObjectMap
insertObject offset object@(PackedObject _ r _) objectMap = let
    getObjectMap'   = Map.insert offset object (getObjectMap   objectMap)
    getObjectIndex' = Map.insert r      offset (getObjectIndex objectMap)
    in ObjectMap getObjectMap' getObjectIndex'

fromBytes :: (Bits t, Integral t) => B.ByteString -> t
fromBytes = B.foldl (\a b -> (a `shiftL` 8) + fromIntegral b) 0

toByteList :: (Bits t, Integral t) => t -> [t]
toByteList n = case divMod n (bit 8) of
    (0, i) -> [i]
    (x, y) -> toByteList x ++ toByteList y

fifthOffsets :: B.ByteString -> [Int]
fifthOffsets ""   = []
fifthOffsets bstr = fromBytes (B.take 8 bstr):fifthOffsets (B.drop 8 bstr)

fixOffsets :: [Int] -> Int -> Int
fixOffsets fOffsets offset
    | offset < msb = offset
    | otherwise    = fOffsets !! (offset-msb)
    where msb = bit 31
