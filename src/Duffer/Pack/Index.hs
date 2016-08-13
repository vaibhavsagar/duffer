module Duffer.Pack.Index where

import qualified Data.ByteString as B

import Prelude hiding (take)

import Data.Bits (Bits, bit, shiftL)

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
    | offset < msb = offset
    | otherwise    = fOffsets !! (offset-msb)
    where msb = bit 31

