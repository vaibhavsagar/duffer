{-# LANGUAGE LambdaCase #-}

module Duffer.Pack.Parser where

import qualified Prelude              as P

import Codec.Compression.Zlib           (decompress)
import Control.Applicative              ((<|>))
import Control.Monad                    (zipWithM)
import Data.Attoparsec.ByteString       (Parser, parseOnly, count, word8
                                        ,takeByteString, takeLazyByteString
                                        ,take, anyWord8, many1, many')
import Data.Attoparsec.ByteString.Char8 (char, space)
import Data.Bits                        (Bits(..))
import Data.Byteable                    (toBytes)
import Data.ByteString                  (ByteString, concat, length, splitAt
                                        ,unpack)
import Data.ByteString.Lazy             (head, drop, toStrict)
import Data.ByteString.UTF8             (fromString)
import Data.Bool                        (bool)
import Data.Functor.Compose             (Compose(..))
import Data.List                        (foldl')
import Data.Map.Strict                  (Map, singleton, union, empty)

import Prelude hiding (take, drop, head, length, concat, splitAt)

import Duffer.Loose.Objects (GitObject, Ref, hashSHA1)
import Duffer.Loose.Parser  (parseBinRef, parseBlob, parseTree, parseCommit
                            ,parseTag, parseRestOfLine, parseHexRef)
import Duffer.Pack.Entries  (PackObjectType(..), WCL(..) ,PackDelta(..)
                            ,PackEntry(..), PackedObject(..), PackIndexEntry(..)
                            ,DeltaInstruction(..), Delta(..), fixOffsets
                            ,fifthOffsets, fromBytes,packObjectType
                            ,getCompressionLevel, FullObjectType(..)
                            ,DeltaObjectType(..))

parsedOnly :: Parser a -> ByteString -> a
parsedOnly parser content = either error id $ parseOnly parser content

hashResolved :: FullObjectType -> ByteString -> Ref
hashResolved t content = hashSHA1 bs'
    where bs'    = concat [header, "\NUL", content]
          header = concat [toBytes t, " ", fromString . show $ length content]

parseResolved :: FullObjectType -> ByteString -> GitObject
parseResolved t = parsedOnly (parseObjectContent t)

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    total     <- parsePackIndexTotal
    refs      <- parsePackIndexRefs total
    crc32s    <- count total parse4Bytes
    offsets   <- count total parse4Bytes
    remaining <- takeByteString
    let (fifth, _)   = splitAt (length remaining - 40) remaining
        fixedOffsets = map (fixOffsets (fifthOffsets fifth)) offsets
    return $ zipWith3 PackIndexEntry fixedOffsets refs crc32s

parsePackIndexUptoRefs :: Parser [Ref]
parsePackIndexUptoRefs = parsePackIndexTotal >>= parsePackIndexRefs

parsePackIndexHeader :: Parser ()
parsePackIndexHeader = traverse word8 (start ++ version) *> pure ()
    where start   = [255, 116, 79, 99]
          version = [0, 0, 0, 2]

parsePackIndexTotal :: Parser Int
parsePackIndexTotal = parsePackIndexHeader *> fmap last parsePackIndexTotals

parsePackIndexTotals :: Parser [Int]
parsePackIndexTotals = count 256 parse4Bytes

parsePackIndexRefs :: Int -> Parser [Ref]
parsePackIndexRefs = flip count parseBinRef

parse4Bytes :: (Bits t, Integral t) => Parser t
parse4Bytes = fromBytes <$> take 4

parsedIndex :: ByteString -> [PackIndexEntry]
parsedIndex = parsedOnly parsePackIndex

parseVarInt :: (Bits t, Integral t) => Parser [t]
parseVarInt = anyWord8 >>= \byte ->
    let value = fromIntegral $ byte .&. 127
    in (value:) <$> bool (return []) parseVarInt (testMSB byte)

testMSB :: Bits t => t -> Bool
testMSB = flip testBit 7

littleEndian, bigEndian :: (Bits t, Integral t) => [t] -> t
littleEndian = foldr  (\a b -> a + (b `shiftL` 7)) 0
bigEndian    = foldl' (\a b -> (a `shiftL` 7) + b) 0

parseOffset :: (Bits t, Integral t) => Parser t
parseOffset = parseVarInt >>= \values -> let len = P.length values - 1 in
    return $ bigEndian values + bool
    -- I think the addition reinstates the MSBs that are otherwise
    -- used to indicate whether there is more of the variable length
    -- integer to parse.
    0 (sum $ map (\i -> 2^(7*i)) [1..len]) (len > 0)

parseTypeLen :: (Bits t, Integral t) => Parser (PackObjectType, t)
parseTypeLen = do
    header <- anyWord8
    let initial  = fromIntegral $ header .&. 15
    size <- (+) initial <$> bool
        (return 0)
        ((`shiftL` 4) <$> (littleEndian <$> parseVarInt))
        (testMSB header)
    return (packObjectType header, size)

parseDeltaInstruction :: Parser DeltaInstruction
parseDeltaInstruction = fromIntegral <$> anyWord8 >>=
    (bool parseInsertInstruction parseCopyInstruction =<< testMSB)

parseInsertInstruction :: Int -> Parser DeltaInstruction
parseInsertInstruction = fmap InsertInstruction . take

parseCopyInstruction :: Bits t => t -> Parser DeltaInstruction
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

parseObjectContent :: FullObjectType -> Parser GitObject
parseObjectContent = \case
    CommitType -> parseCommit
    TreeType   -> parseTree
    BlobType   -> parseBlob
    TagType    -> parseTag

parseWCL :: Parser (WCL ByteString)
parseWCL = takeLazyByteString >>= \compressed -> return $ WCL
    (getCompressionLevel . head . drop 1 $ compressed)
    (toStrict . decompress               $ compressed)

parseFullObject
    :: Parser (WCL ByteString) -> FullObjectType -> Parser PackedObject
parseFullObject parser objType = parser >>= \decompressed ->
    let ref = hashResolved objType $ wclContent decompressed
    in return $ PackedObject objType ref decompressed

parseOfsDelta, parseRefDelta :: Parser (WCL ByteString) -> Parser PackDelta
parseOfsDelta parser = OfsDelta <$> parseOffset <*> parseWCLDelta parser
parseRefDelta parser = RefDelta <$> parseBinRef <*> parseWCLDelta parser

parseWCLDelta :: Parser (WCL ByteString) -> Parser (WCL Delta)
parseWCLDelta = getCompose . fmap (parsedOnly parseDelta) . Compose

parsePackRegion :: Parser PackEntry
parsePackRegion = parsePackRegion' parseWCL

parsePackRegion' :: Parser (WCL ByteString) -> Parser PackEntry
parsePackRegion' parser =
    fst <$> (parseTypeLen :: Parser (PackObjectType, Int)) >>= \case
        DeltaType OfsDeltaType -> UnResolved <$> parseOfsDelta   parser
        DeltaType RefDeltaType -> UnResolved <$> parseRefDelta   parser
        FullType fType         -> Resolved   <$> parseFullObject parser fType

parsedPackRegion :: ByteString -> PackEntry
parsedPackRegion = parsedOnly parsePackRegion

parsedPackIndexRefs :: ByteString -> [Ref]
parsedPackIndexRefs = parsedOnly parsePackIndexUptoRefs

parsePackFileHeader :: Parser Int
parsePackFileHeader = mapM word8 (unpack "PACK") *> take 4 *> parse4Bytes

parsePackRefsHeader, parseCaret, parsePackRef :: Parser (Map ByteString Ref)
parsePackRefsHeader = char '#' *> parseRestOfLine *> return empty
parseCaret          = char '^' *> parseRestOfLine *> return empty
parsePackRef        =
    flip singleton <$> (parseHexRef <* space) <*> parseRestOfLine

parsePackRefs :: Parser (Map ByteString Ref)
parsePackRefs = parsePackRefsHeader
    >> foldr union empty <$> many' (parseCaret <|> parsePackRef)

parsedPackRefs :: ByteString -> Map ByteString Ref
parsedPackRefs = parsedOnly parsePackRefs
