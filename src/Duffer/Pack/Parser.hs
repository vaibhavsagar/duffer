module Duffer.Pack.Parser where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec.ByteString
import Data.Bits

import Codec.Compression.Zlib (decompress)
import Control.Monad          (zipWithM)
import GHC.Word               (Word8)

import Prelude hiding (take)

import Duffer.Loose.Objects
import Duffer.Loose.Parser (parseBinRef, parseBlob, parseTree, parseCommit
                           ,parseTag)
import Duffer.Pack.Entries

hashResolved :: PackObjectType -> PackCompressed B.ByteString -> Ref
hashResolved t = hash . parseResolved t

parseResolved :: PackObjectType -> PackCompressed B.ByteString -> GitObject
parseResolved t (PackCompressed _ source) =
    either error id $ parseOnly (parseObjectContent t) source

word8s :: [Word8] -> Parser [Word8]
word8s = mapM word8

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    -- these are fixed parts of the index? might be worth pulling
    -- them out into named constants to indicate this if that's the case
    header    <- word8s [255, 116, 79, 99]
    version   <- word8s [0, 0, 0, 2]
    totals    <- count 256 parse4Bytes
    let total =  count (last totals)
    -- it feels a bit weird to be referencing things from Loose.Parser
    -- like this. I imagine this is because you developed the Loose
    -- parser before this one and then just imported the stuff that
    -- was the same from there :) in terms of package organization, it
    -- might be worth pulling things that are shared into their own
    -- module and importing them from there in both Parsers
    refs      <- total parseBinRef
    crc32s    <- total parse4Bytes
    offsets   <- total parse4Bytes
    remaining <- takeByteString
    let (fifth, checks) = B.splitAt (B.length remaining - 40) remaining
    let fixedOffsets    = map (fixOffsets (fifthOffsets fifth)) offsets
    return $ zipWith3 PackIndexEntry fixedOffsets refs crc32s

parse4Bytes :: (Bits t, Integral t) => Parser t
parse4Bytes = fromBytes <$> take 4

parsedIndex :: B.ByteString -> [PackIndexEntry]
parsedIndex = either error id . parseOnly parsePackIndex

parseVarInt :: (Bits t, Integral t) => Parser [t]
parseVarInt = do
  byte <- anyWord8
  let value = fromIntegral $ byte .&. 127
      -- a general observation: there's a fair amount of `testBit
      -- whatever 7` in this file, and also a fair amount of if ... then ... else.
      --
      -- they also tend to be clustered together! this feels like it
      -- might be a code smell? i feel like the "haskell way" is to
      -- lift stuff into the type and make decisions by pattern
      -- matching. here that might take the form of two newtype
      -- wrapers for Word8, one of which is for things where the 7th
      -- bit is set, and other for things where it's not?
      --
      -- i'm not sure if this would even work, but is seems like it
      -- would be nice to abstract the pattern of "do this thing if
      -- the 7th bit is set, this thing if it's not" somewhere so that
      -- there aren't as many if expressions
      more  = testBit byte 7
  (value:) <$> if more then parseVarInt else return []

littleEndian, bigEndian :: (Bits t, Integral t) => [t] -> t
littleEndian = foldr (\a b -> a + (b `shiftL` 7)) 0
bigEndian    = foldl (\a b -> (a `shiftL` 7) + b) 0

parseOffset :: (Bits t, Integral t) => Parser t
parseOffset = do
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
    <$>  getVarInt [0..3] [0,8..24]
    <*> (getVarInt [4..6] [0,8..16] >>= \len ->
        return $ if len == 0 then 0x10000 else len)
    -- this feels messy, it's definitely hard to read :/ i'm not sure
    -- what the best way to fix this is. maybe pull getVarInt and
    -- readShift out into their own functions? that feels weird though
    -- since they're very special purpose . . .
    where getVarInt bits shifts = foldr (.|.) 0 <$>
            zipWithM readShift (map (testBit byte) bits) shifts
          readShift more shift = if more
            then (`shiftL` shift) <$> (fromIntegral <$> anyWord8)
            else return 0

parseDelta :: Parser Delta
-- sick applicative usage, 10/10
parseDelta = Delta <$> len <*> len <*> many1 parseDeltaInstruction
    where len = littleEndian <$> parseVarInt

parseObjectContent :: PackObjectType -> Parser GitObject
parseObjectContent t = case t of
    CommitObject -> parseCommit
    TreeObject   -> parseTree
    BlobObject   -> parseBlob
    TagObject    -> parseTag
    -- bummer that this can fail at runtime :( maybe overkill, but
    -- maybe would have different datatypes for resolved and
    -- unresolved pack objects and then have PackObjectType be a
    -- typeclass w/ all the operations that need to work on both?
    _            -> error "deltas must be resolved first"


-- its confused that a function called parseDecompressed returns a
-- PackCompressed. I think of parse as being called parseX and having
-- type Parser X, so this one strikes me as a bit odd. why isn't it
-- called parseCompressed?
parseDecompressed :: Parser (PackCompressed B.ByteString)
parseDecompressed = do
    compressed       <- takeLazyByteString
    let level        =  getCompressionLevel $ L.head $ L.drop 1 compressed
    let decompressed =  L.toStrict $ decompress compressed
    return $ PackCompressed level decompressed

parseFullObject :: PackObjectType -> Parser PackedObject
parseFullObject objectType = do
    decompressed <- parseDecompressed
    let ref = hashResolved objectType decompressed
    return $ PackedObject objectType ref decompressed

parseOfsDelta, parseRefDelta :: Parser PackDelta
parseOfsDelta = OfsDelta <$> parseOffset <*> parseDecompressedDelta
parseRefDelta = RefDelta <$> parseBinRef <*> parseDecompressedDelta


-- same as above re: the compressed / decompressed thing
parseDecompressedDelta :: Parser (PackCompressed Delta)
parseDecompressedDelta = do
    packCompressed <- parseDecompressed
    return $ (either error id . parseOnly parseDelta) <$> packCompressed

parsePackRegion :: Parser PackEntry
parsePackRegion = do
    -- having the type assertion here feels bad to me, but I tried a
    -- bunch of stuff and couldn't figure out a better way to do it :/
    (objectType, _) <- parseTypeLen :: Parser (PackObjectType, Int)
    case objectType of
        t | fullObject t -> Resolved   <$> parseFullObject objectType
        OfsDeltaObject   -> UnResolved <$> parseOfsDelta
        RefDeltaObject   -> UnResolved <$> parseRefDelta
        _                -> error "unrecognised type"

parsedPackRegion :: B.ByteString -> PackEntry
parsedPackRegion = either error id . parseOnly parsePackRegion

-- reading the spec, this header is the first thing I see, so when
-- reading here it's a bit confusing to me that this function isn't
-- referenced from any other function in this module. my brain wants
-- to find a top level "parsePackfile" function (from ByteString ->
-- Either Error Packfile of smth like that), which goes like
-- "parsePackfileHeader, then . . ., etc., etc."
--
-- even if you don't actually use it typically because you're
-- streaming one part at a time or w/e it would probably be good to
-- have written down here if only as an aid to the reader to figure
-- out what order they should be looking at the code in. or if there
-- are really are multiple entry points, move them to the top and have
-- a comment indicating them as such?
parsePackfileHeader :: Parser Int
parsePackfileHeader =
    word8s (B.unpack "PACK") *> take 4 *> (fromBytes <$> take 4)
