module Duffer.PackIndex where

import qualified Data.ByteString as B

import Prelude hiding (take)

import Data.Bits
import Data.Attoparsec.ByteString
import Data.ByteString.Base16 (encode)
import GHC.Word (Word8)

data PackIndexEntry
    = PackIndexEntry { packIndexEntryRef    :: B.ByteString
                     , packIndexEntryOffset :: Int
                     }

word8s :: [Word8] -> Parser [Word8]
word8s = mapM word8

fromBytes :: (Bits t, Integral t) => [t] -> t
fromBytes = foldl (\a b -> (fromIntegral a `shiftL` 8) + b) 0

toBytes :: (Bits t, Integral t) => t -> [t]
toBytes n = case divMod n (bit 8) of
    (0, i) -> [i]
    (x, y) -> toBytes x ++ toBytes y

fromPack :: (Bits t, Integral t) => B.ByteString -> t
fromPack = fromBytes . map (fromIntegral . toInteger) . B.unpack

fifthOffsets :: B.ByteString -> [Int]
fifthOffsets ""   = []
fifthOffsets bstr = fromPack (B.take 8 bstr):fifthOffsets (B.drop 8 bstr)

fixOffsets :: [Int] -> Int -> Int
fixOffsets fOffsets offset
    | offset >= msb = fOffsets !! (offset-msb)
    | otherwise     = offset
    where msb = bit 7 `shiftL` (8*3)

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    header    <- word8s [255, 116, 79, 99]
    version   <- word8s [0, 0, 0, 2]
    totals    <- count 256 (fromPack <$> take 4)
    let total =  count (last totals)
    names     <- total $ encode <$> take 20
    checks    <- total $ take 4
    offsets   <- total $ fromPack <$> take 4
    remaining <- takeByteString
    let (fifth, checks) = B.splitAt (B.length remaining - 40) remaining
    let fixedOffsets    = if fifth /= B.empty
        then map (fixOffsets (fifthOffsets fifth)) offsets
        else offsets
    return $ zipWith PackIndexEntry names fixedOffsets
