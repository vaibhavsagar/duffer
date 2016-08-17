module Duffer.Pack.Entries where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Byteable
import Data.ByteString.Base16 (decode)
import Data.Bits
import Data.Word (Word32)

import Duffer.Loose.Objects (Ref)
import Duffer.Loose (compress)

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

instance Byteable PackDelta where
    toBytes (RefDelta ref delta) = let
        encodedRef = fst $ decode ref
        compressed = compress (toBytes delta)
        in B.append encodedRef compressed
    toBytes (OfsDelta offset delta) = let
        encodedOffset = encodeOffset offset
        compressed = compress (toBytes delta)
        in B.append encodedOffset compressed

encodeOffset :: Int -> B.ByteString
encodeOffset n = let
    -- This is the number of terms of 2^7+2^14+..+2^(7i) that we need to
    -- subtract from our number before encoding it
    noTerms = floor $ recip $ logBase (fromIntegral n) (2^7) :: Int
    remove  = sum $ map (\i -> 2^(7*i)) [1..noTerms]
    bytes   = to7BitList $ n - remove
    bytes'  = if noTerms == 1 && length bytes == 1 then 0:bytes else bytes
    bytes'' = map (`setBit` 7) (init bytes') ++ [last bytes']
    in B.pack $ map fromIntegral bytes''

instance Byteable Delta where
    toBytes (Delta source dest instructions) = let
        sourceEncoded = toLittleEndian $ to7BitList source
        destEncoded   = toLittleEndian $ to7BitList dest
        instrsBS      = B.concat (map toBytes instructions)
        in B.concat [sourceEncoded, destEncoded, instrsBS]

toLittleEndian :: (Bits t, Integral t) => [t] -> B.ByteString
toLittleEndian nums = case nums of
    (n:ns) -> B.pack $ map fromIntegral $ reverse $ n:map (`setBit` 7) ns
    []     -> ""

instance Byteable DeltaInstruction where
    toBytes instruction = case instruction of
        InsertInstruction content     ->
            B.singleton (fromIntegral $ B.length content) `B.append` content
        CopyInstruction offset length -> let
            offsetBytes = toByteList offset
            lenBytes    = toByteList length
            offsetBits  = map (>0) offsetBytes
            lenBits     = map (>0) lenBytes
            encodedOff  = encode offsetBytes
            encodedLen  = encode lenBytes
            bools       = True:padFalse lenBits 3 ++ padFalse offsetBits 4
            firstByte   = boolsToByte bools 0
            in B.concat [B.singleton firstByte, encodedOff, encodedLen]
        where encode = B.pack . map fromIntegral . reverse . filter (>0)
              padFalse :: [Bool] -> Int -> [Bool]
              padFalse bits len = let
                  pad = len - length bits
                  in if pad > 0
                      then replicate pad False ++ bits
                      else bits
              boolsToByte :: (Bits a, Num a) => [Bool] -> a -> a
              boolsToByte []     acc = acc
              boolsToByte (x:xs) acc = let
                  value = if x then bit (length xs) else 0
                  in boolsToByte xs (value + acc)

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
    (0, i) -> [fromIntegral i]
    (x, y) -> toByteList x ++ toByteList y

to7BitList :: (Bits t, Integral t) => t -> [t]
to7BitList n = case divMod n (bit 7) of
    (0, i) -> [fromIntegral i]
    (x, y) -> to7BitList x ++ to7BitList y

fifthOffsets :: B.ByteString -> [Int]
fifthOffsets ""   = []
fifthOffsets bstr = fromBytes (B.take 8 bstr):fifthOffsets (B.drop 8 bstr)

fixOffsets :: [Int] -> Int -> Int
fixOffsets fOffsets offset
    | offset < msb = offset
    | otherwise    = fOffsets !! (offset-msb)
    where msb = bit 31
