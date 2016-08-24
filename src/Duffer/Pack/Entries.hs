module Duffer.Pack.Entries where

import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString        as B
import qualified Data.Map.Strict        as Map

import Data.Byteable
import Data.ByteString.Base16 (decode)
import Data.ByteString.Lazy (fromStrict, toStrict)
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
    = OfsDelta Int (PackCompressed Delta)
    | RefDelta Ref (PackCompressed Delta)
    deriving (Show, Eq)

data PackedObject =
    PackedObject PackObjectType Ref (PackCompressed B.ByteString)
    deriving (Show, Eq)

data PackEntry = Resolved PackedObject | UnResolved PackDelta
    deriving (Show, Eq)

data PackCompressed a = PackCompressed
    { packCLevel   :: Z.CompressionLevel
    , packCContent :: a
    } deriving (Show, Eq)

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

isResolved :: PackEntry -> Bool
isResolved (Resolved _)   = True
isResolved (UnResolved _) = False

instance Byteable PackEntry where
    toBytes (Resolved  packedObject)         = toBytes packedObject
    toBytes (UnResolved ofsD@(OfsDelta _ delta@(PackCompressed l d))) = let
        header = encodeTypeLen OfsDeltaObject $ B.length (toBytes d)
        in header `B.append` toBytes ofsD
    toBytes (UnResolved refD@(RefDelta _ delta@(PackCompressed l d))) = let
        header = encodeTypeLen RefDeltaObject $ B.length (toBytes d)
        in header `B.append` toBytes refD

instance Byteable PackedObject where
    toBytes (PackedObject t _ packed) = let
        header     = encodeTypeLen t $ B.length $ packCContent packed
        compressed = toBytes packed
        in header `B.append` compressed

instance (Byteable a) => Byteable (PackCompressed a) where
    toBytes (PackCompressed level content) =
        compressToLevel level $ toBytes content

compressToLevel :: Z.CompressionLevel -> B.ByteString -> B.ByteString
compressToLevel level content = toStrict $
    Z.compressWith Z.defaultCompressParams
      { Z.compressLevel = level }
      $ fromStrict content

getCompressionLevel :: B.ByteString -> Z.CompressionLevel
getCompressionLevel bytes = let
    levelByte = fromIntegral $ B.head $ B.tail bytes
    in case levelByte of
        1   -> Z.bestSpeed
        156 -> Z.defaultCompression

instance Functor PackCompressed where
    fmap f (PackCompressed level content) =
        PackCompressed level (f content)

encodeTypeLen :: PackObjectType -> Int -> B.ByteString
encodeTypeLen packObjectType len = let
    (last4, rest) = packEntryLenList len
    firstByte     = (fromEnum packObjectType `shiftL` 4) .|. last4
    firstByte'    = if rest /= "" then setBit firstByte 7 else firstByte
    in B.cons (fromIntegral firstByte') rest

packEntryLenList :: Int -> (Int, B.ByteString)
packEntryLenList n = let
    last4  = fromIntegral n .&. 15
    rest   = fromIntegral n `shiftR` 4 :: Int
    last4' = if rest > 0
        then setBit last4 7
        else last4
    restL  = to7BitList rest
    restL' = if restL /= [0]
        then map fromIntegral $ head restL:map (`setBit` 7) (tail restL)
        else []
    in (last4', B.pack $ reverse restL')

instance Byteable PackDelta where
    toBytes (RefDelta ref delta) = let
        encodedRef = fst $ decode ref
        compressed = toBytes delta
        in B.append encodedRef compressed
    toBytes (OfsDelta offset delta) = let
        encodedOffset = encodeOffset offset
        compressed = toBytes delta
        in B.append encodedOffset compressed

encodeOffset :: Int -> B.ByteString
encodeOffset n = let
    {- Given a = r = 2^7:
     - x           = a((1 - r^n)/(1-r))
     - x - xr      = a - ar^n
     - x + ar^n    = a + xr
     - x + r^(n+1) = r + xr
     - r^(n+1)     = r + xr -x
     - r^(n+1)     = x(r-1) + r
     - n+1         = log128 x(r-1) + r
     - n           = floor ((log128 x(2^7-1) + 2^7) - 1)
     -}
    noTerms     = floor $ logBase (2^7) (fromIntegral n * (2^7 - 1) + 2^7) - 1
    remove      = sum $ take noTerms $ map (\i -> 2^(7*i)) [1..]
    remainder   = n - remove
    varInt      = to7BitList remainder
    encodedInts = setMSBs $ leftPadZeros varInt (noTerms + 1)
    in B.pack $ map fromIntegral encodedInts

leftPadZeros :: [Int] -> Int -> [Int]
leftPadZeros ints n
    | length ints >= n = ints
    | otherwise        = leftPadZeros (0:ints) n

setMSBs :: [Int] -> [Int]
setMSBs ints = let
    ints'  = reverse ints
    ints'' = head ints' : map (`setBit` 7) ( tail ints')
    in reverse ints''

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

