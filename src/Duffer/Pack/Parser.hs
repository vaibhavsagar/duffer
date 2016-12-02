module Duffer.Pack.Parser where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict      as M


import Codec.Compression.Zlib           (decompress)
import Control.Applicative              ((<|>))
import Control.Monad                    (zipWithM)
import Data.Attoparsec.ByteString       (Parser(..), parseOnly, count, word8
                                        ,takeByteString, takeLazyByteString
                                        ,take, anyWord8, many1, many')
import Data.Attoparsec.ByteString.Char8 (char, space)
import Data.Bits                        (Bits(..))
import Data.Bool                        (bool)
import Data.List                        (foldl')
import GHC.Word                         (Word8)

import Prelude hiding (take)

import Duffer.Loose.Objects (GitObject(..), Ref, hash)
import Duffer.Loose.Parser  (parseBinRef, parseBlob, parseTree, parseCommit
                            ,parseTag, parseRestOfLine, parseHexRef)
import Duffer.Pack.Entries  (PackObjectType(..), PackDecompressed(..)
                            ,PackDelta(..), PackEntry(..), PackedObject(..)
                            ,PackIndexEntry(..), DeltaInstruction(..)
                            ,Delta(..), fixOffsets, fifthOffsets, fromBytes
                            ,packObjectType, getCompressionLevel, fullObject)

hashResolved :: PackObjectType -> PackDecompressed B.ByteString -> Ref
hashResolved t = hash . parseResolved t

parseResolved :: PackObjectType -> PackDecompressed B.ByteString -> GitObject
parseResolved t (PackDecompressed _ source) =
    either error id $ parseOnly (parseObjectContent t) source

word8s :: [Word8] -> Parser [Word8]
word8s = mapM word8

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    total     <- parsePackIndexHeader *> fmap last parsePackIndexTotals
    refs      <- parsePackIndexRefs total
    crc32s    <- count total parse4Bytes
    offsets   <- count total parse4Bytes
    remaining <- takeByteString
    let (fifth, _) = B.splitAt (B.length remaining - 40) remaining
    let fixedOffsets    = map (fixOffsets (fifthOffsets fifth)) offsets
    return $ zipWith3 PackIndexEntry fixedOffsets refs crc32s

parsePackIndexUptoRefs :: Parser [Ref]
parsePackIndexUptoRefs = parsePackIndexHeader
    *> fmap last parsePackIndexTotals >>= parsePackIndexRefs

parsePackIndexHeader :: Parser ()
parsePackIndexHeader = word8s start *> word8s version *> pure ()
    where start   = [255, 116, 79, 99]
          version = [0, 0, 0, 2]

parsePackIndexTotals :: Parser [Int]
parsePackIndexTotals = count 256 parse4Bytes

parsePackIndexRefs :: Int -> Parser [Ref]
parsePackIndexRefs = flip count parseBinRef

parse4Bytes :: (Bits t, Integral t) => Parser t
parse4Bytes = fromBytes <$> take 4

parsedIndex :: B.ByteString -> [PackIndexEntry]
parsedIndex = either error id . parseOnly parsePackIndex

parseVarInt :: (Bits t, Integral t) => Parser [t]
parseVarInt = anyWord8 >>= \byte ->
    let value = fromIntegral $ byte .&. 127
        more  = testMSB byte
    in (value:) <$> bool (return []) parseVarInt more

testMSB :: Bits t => t -> Bool
testMSB = flip testBit 7

littleEndian, bigEndian :: (Bits t, Integral t) => [t] -> t
littleEndian = foldr  (\a b -> a + (b `shiftL` 7)) 0
bigEndian    = foldl' (\a b -> (a `shiftL` 7) + b) 0

parseOffset :: (Bits t, Integral t) => Parser t
parseOffset = parseVarInt >>= \values ->
    let len          = length values - 1
        concatenated = bigEndian values
    in return $ concatenated + bool
        -- I think the addition reinstates the MSBs that are otherwise
        -- used to indicate whether there is more of the variable length
        -- integer to parse.
        0
        (sum $ map (\i -> 2^(7*i)) [1..len])
        (len > 0)

parseTypeLen :: (Bits t, Integral t) => Parser (PackObjectType, t)
parseTypeLen = do
    header <- anyWord8
    let initial  = fromIntegral $ header .&. 15
    let packType = packObjectType header
    size <- (+) initial <$> bool
        (return 0)
        ((`shiftL` 4) <$> (littleEndian <$> parseVarInt))
        (testMSB header)
    return (packType, size)

parseDeltaInstruction :: Parser DeltaInstruction
parseDeltaInstruction = fromIntegral <$> anyWord8 >>= \instruction ->
    bool parseInsertInstruction parseCopyInstruction (testMSB instruction)
        instruction

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
    <$>  getVarInt [0..3] [0,8..24]
    <*> (getVarInt [4..6] [0,8..16] >>= \len ->
        return $ bool len 0x10000 (len == 0))
    where getVarInt bits shifts = foldr (.|.) 0 <$>
            zipWithM readShift shifts (map (testBit byte) bits)
          readShift shiftN = bool
            (return 0)
            ((`shiftL` shiftN) <$> (fromIntegral <$> anyWord8))

parseDelta :: Parser Delta
parseDelta = Delta <$> len <*> len <*> many1 parseDeltaInstruction
    where len = littleEndian <$> parseVarInt

parseObjectContent :: PackObjectType -> Parser GitObject
parseObjectContent t = case t of
    CommitObject -> parseCommit
    TreeObject   -> parseTree
    BlobObject   -> parseBlob
    TagObject    -> parseTag
    _            -> error "deltas must be resolved first"

parseDecompressed :: Parser (PackDecompressed B.ByteString)
parseDecompressed = takeLazyByteString >>= \compressed ->
    let level        = getCompressionLevel $ L.head $ L.drop 1 compressed
        decompressed = L.toStrict $ decompress compressed
    in return $ PackDecompressed level decompressed

parseFullObject :: PackObjectType -> Parser PackedObject
parseFullObject objType = parseDecompressed >>= \decompressed ->
    let ref = hashResolved objType decompressed
    in return $ PackedObject objType ref decompressed

parseOfsDelta, parseRefDelta :: Parser PackDelta
parseOfsDelta = OfsDelta <$> parseOffset <*> parseDecompressedDelta
parseRefDelta = RefDelta <$> parseBinRef <*> parseDecompressedDelta

parseDecompressedDelta :: Parser (PackDecompressed Delta)
parseDecompressedDelta = parseDecompressed >>= \packCompressed ->
    return $ (either error id . parseOnly parseDelta) <$> packCompressed

parsePackRegion :: Parser PackEntry
parsePackRegion = do
    (objType, _) <- parseTypeLen :: Parser (PackObjectType, Int)
    case objType of
        t | fullObject t -> Resolved   <$> parseFullObject objType
        OfsDeltaObject   -> UnResolved <$> parseOfsDelta
        RefDeltaObject   -> UnResolved <$> parseRefDelta
        _                -> error "unrecognised type"

parsedPackRegion :: B.ByteString -> PackEntry
parsedPackRegion = either error id . parseOnly parsePackRegion

parsedPackIndexRefs :: B.ByteString -> [Ref]
parsedPackIndexRefs = either error id . parseOnly parsePackIndexUptoRefs

parsePackFileHeader :: Parser Int
parsePackFileHeader =
    word8s (B.unpack "PACK") *> take 4 *> (fromBytes <$> take 4)

parsePackRefsHeader, parseCaret, parsePackRef :: Parser (M.Map B.ByteString Ref)
parsePackRefsHeader = char '#' *> parseRestOfLine *> return M.empty
parseCaret          = char '^' *> parseRestOfLine *> return M.empty
parsePackRef        =
    flip M.singleton <$> (parseHexRef <* space) <*> parseRestOfLine

parsePackRefs :: Parser (M.Map B.ByteString Ref)
parsePackRefs = parsePackRefsHeader
    >> foldr M.union M.empty <$> many' (parseCaret <|> parsePackRef)

parsedPackRefs :: B.ByteString -> M.Map B.ByteString Ref
parsedPackRefs = either error id . parseOnly parsePackRefs
