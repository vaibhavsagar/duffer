module Duffer.Pack.Bits (module Duffer.Pack.Bits) where

import qualified Data.ByteString as B
import Data.Bits
import Data.Bool (bool)
import Data.Word (Word8)

import Duffer.Misc ((.:))

toBitList :: (Bits t, Integral t) => Int -> t -> [t]
toBitList = reverse .: go
    where go some n = case divMod n (bit some) of
            (0, i) -> [fromIntegral i]
            (x, y) ->  fromIntegral y : go some x

toByteList, to7BitList :: (Bits t, Integral t) => t -> [t]
toByteList = toBitList 8
to7BitList = toBitList 7

fromBytes :: (Bits t, Integral t) => B.ByteString -> t
fromBytes = B.foldl' (\a b -> (a `shiftL` 8) + fromIntegral b) 0

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
encodeOffset :: Int -> B.ByteString
encodeOffset n = let
    noTermsLog = logBase 128 (fromIntegral n * (128 - 1) + 128) :: Double
    noTerms    = floor noTermsLog - 1
    powers128  = map (128^) ([1..] :: [Integer])
    remove     = sum $ take noTerms powers128 :: Integer
    remainder  = n - fromIntegral remove :: Int
    varInt     = to7BitList remainder
    in toLittleEndian . reverse $ leftPadZeros varInt (noTerms + 1)

leftPadZeros :: [Int] -> Int -> [Int]
leftPadZeros ints n
    | length ints < n = leftPadZeros (0:ints) n
    | otherwise       = ints

setMSB :: Bits t => t -> t
setMSB = (`setBit` 7)

setMSBs :: (Bits t, Integral t) => [t] -> [Word8]
setMSBs []     = []
setMSBs (i:is) = map fromIntegral $ i : map setMSB is

toLittleEndian :: (Bits t, Integral t) => [t] -> B.ByteString
toLittleEndian = B.pack . reverse . setMSBs

packEntryLenList :: Int -> (Int, B.ByteString)
packEntryLenList n = let
    rest   = fromIntegral n `shiftR` 4 :: Int
    last4  = fromIntegral n .&. 15
    last4' = bool last4 (setMSB last4) (rest > 0)
    restL  = to7BitList rest
    restL' = bool B.empty (toLittleEndian restL) (restL /= [0])
    in (last4', restL')

fifthOffsets :: B.ByteString -> [Int]
fifthOffsets ""   = []
fifthOffsets bstr = fromBytes (B.take 8 bstr):fifthOffsets (B.drop 8 bstr)

fixOffsets :: [Int] -> Int -> Int
fixOffsets fOffsets offset
    | offset < msb = offset
    | otherwise    = fOffsets !! (offset-msb)
    where msb = bit 31
