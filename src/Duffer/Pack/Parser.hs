module Duffer.Pack.Parser where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec.ByteString
import Data.Bits

import Codec.Compression.Zlib (decompress)
import Control.Monad (zipWithM)
import GHC.Word               (Word8)

import Prelude hiding (take)

import Duffer.Loose.Objects
import Duffer.Loose.Parser
    (parseBinRef, parseBlob, parseTree, parseCommit, parseTag)
import Duffer.Pack.Index
import Duffer.Pack.Entries

hashResolved :: PackObjectType -> B.ByteString -> Ref
hashResolved t = hash . parseResolved t

parseResolved :: PackObjectType -> B.ByteString -> GitObject
parseResolved t = either error id . parseOnly (parseObjectContent t)

word8s :: [Word8] -> Parser [Word8]
word8s = mapM word8

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    header    <- word8s [255, 116, 79, 99]
    version   <- word8s [0, 0, 0, 2]
    totals    <- count 256 (fromPack <$> take 4)
    let total =  count (last totals)
    refs      <- total   parseBinRef
    checks    <- total $ take 4
    offsets   <- total $ fromPack    <$> take 4
    remaining <- takeByteString
    let (fifth, checks) = B.splitAt (B.length remaining - 40) remaining
    let fixedOffsets    = if fifth /= B.empty
        then map (fixOffsets (fifthOffsets fifth)) offsets
        else offsets
    return $ zipWith PackIndexEntry fixedOffsets refs

parsedIndex :: B.ByteString -> [PackIndexEntry]
parsedIndex = either error id . parseOnly parsePackIndex

parseVarInt :: (Bits t, Integral t) => Parser [t]
parseVarInt = do
    byte <- anyWord8
    let value = fromIntegral $ byte .&. 127
    if byte `testBit` 7
        then do
            rest <- parseVarInt
            return (value:rest)
        else
            return [value]

littleEndian, bigEndian :: (Bits t, Integral t) => [t] -> t
littleEndian = foldr (\a b -> a + (b `shiftL` 7)) 0
bigEndian    = foldl (\a b -> (a `shiftL` 7) + b) 0

parseOffsetEncoded :: (Bits t, Integral t) => Parser t
parseOffsetEncoded = do
    values <- parseVarInt
    let len = length values - 1
    let concatenated = bigEndian values
    return $ concatenated + if len > 0
        -- I think the addition reinstates the MSBs that are otherwise
        -- used to indicate whether there is more of the variable length
        -- integer to parse.
        then sum $ map (\i -> 2^(7*i)) [1..len]
        else 0

parseTypeLen :: (Bits t, Integral t) => Parser (PackObjectType, t)
parseTypeLen = do
    header <- anyWord8
    let packType = packObjectType header
    let initial  = fromIntegral $ header .&. 15
    size <- if header `testBit` 7
        then do
            rest <- littleEndian <$> parseVarInt
            return $ initial + (rest `shiftL` 4)
        else
            return initial
    return (packType, size)

parseDeltaInstruction :: Parser DeltaInstruction
parseDeltaInstruction = do
    instruction <- fromIntegral <$> anyWord8
    if instruction `testBit` 7
        then parseCopyInstruction   instruction
        else parseInsertInstruction instruction

parseInsertInstruction :: Int -> Parser DeltaInstruction
parseInsertInstruction len = InsertInstruction <$> take len

parseCopyInstruction :: (Bits t, Integral t) => t -> Parser DeltaInstruction
parseCopyInstruction byte = CopyInstruction
    {-
    o0 <- readShift (testBit byte 0) 0
    o1 <- readShift (testBit byte 1) 8
    o2 <- readShift (testBit byte 2) 16
    o3 <- readShift (testBit byte 3) 24

    l0 <- readShift (testBit byte 4) 0
    l1 <- readShift (testBit byte 5) 8
    l2 <- readShift (testBit byte 6) 16

    let offset = o0 .|. o1 .|. o2 .|. o3
    let length = l0 .|. l1 .|. l2
    -}
    <$> getVarInt [0..3] [0,8..24]
    <*> getVarInt [4..6] [0,8..16]
    where getVarInt bits shifts = foldr (.|.) 0 <$>
            zipWithM readShift (map (testBit byte) bits) shifts
          readShift cond shift = if cond
            then do
                nextByte <- fromIntegral <$> anyWord8
                return $ nextByte `shiftL` shift
            else return 0

parseDelta :: Parser Delta
parseDelta = Delta
    <$> (littleEndian <$> parseVarInt)
    <*> (littleEndian <$> parseVarInt)
    <*> many1 parseDeltaInstruction

parseObjectContent :: PackObjectType -> Parser GitObject
parseObjectContent t = case t of
    CommitObject -> parseCommit
    TreeObject   -> parseTree
    BlobObject   -> parseBlob
    TagObject    -> parseTag
    _            -> error "deltas must be resolved first"

parseDecompressed :: Parser B.ByteString
parseDecompressed = L.toStrict . decompress <$> takeLazyByteString

parseFullObject :: PackObjectType -> Parser PackEntry
parseFullObject objectType = do
    decompressed <- parseDecompressed
    let ref = hashResolved objectType decompressed
    return $ PackedObject objectType ref decompressed

parseOfsDelta :: Parser PackDelta
parseOfsDelta = OfsDelta
    <$> parseOffsetEncoded
    <*> (parseOnlyDelta <$> parseDecompressed)

parseRefDelta :: Parser PackDelta
parseRefDelta = RefDelta
    <$> parseBinRef
    <*> (parseOnlyDelta <$> parseDecompressed)

parseOnlyDelta :: B.ByteString -> Delta
parseOnlyDelta = either error id . parseOnly parseDelta

parsePackRegion :: Parser PackEntry
parsePackRegion = do
    (objectType, len) <- parseTypeLen :: Parser (PackObjectType, Int)
    case objectType of
        t | fullObject t -> parseFullObject objectType
        OfsDeltaObject   -> PackedDelta <$> parseOfsDelta
        RefDeltaObject   -> PackedDelta <$> parseRefDelta
        _                -> error $ "wrong object type: " ++ show objectType
