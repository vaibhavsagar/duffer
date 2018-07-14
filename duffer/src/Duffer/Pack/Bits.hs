module Duffer.Pack.Bits (module Duffer.Pack.Bits) where

import qualified Data.ByteString as B
import Data.Bits
import Data.Bool       (bool)
import qualified Data.DList as D
import Data.Word       (Word8)
import Numeric.Natural (Natural)

import Duffer.Misc ((.:))

toBitList :: (Bits t, Integral t) => Int -> t -> [t]
toBitList = D.toList .: go
    where go some n = case divMod n (bit some) of
            (0, i) -> D.singleton $      fromIntegral i
            (x, y) -> go some x `D.snoc` fromIntegral y

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
encodeOffset :: Natural -> B.ByteString
encodeOffset n = let
    noTermsLog = logBase 128 (fromIntegral n * (128 - 1) + 128) :: Double
    noTerms    = floor noTermsLog - 1 :: Int
    powers128  = map (128^) ([1..] :: [Integer])
    remove     = sum $ take noTerms powers128 :: Natural
    remainder  = n - fromIntegral remove :: Natural
    varInt     = to7BitList remainder
    in toLittleEndian . reverse $ leftPadZeros varInt (noTerms + 1)

leftPadZeros :: [Natural] -> Int -> [Natural]
leftPadZeros ints n
    | length ints < n = replicate (n - length ints) 0 ++ ints
    | otherwise       = ints

setMSB :: Bits t => t -> t
setMSB = (`setBit` 7)

setMSBs :: (Bits t, Integral t) => [t] -> [Word8]
setMSBs []     = []
setMSBs (i:is) = map fromIntegral $ i : map setMSB is

toLittleEndian :: (Bits t, Integral t) => [t] -> B.ByteString
toLittleEndian = B.pack . reverse . setMSBs

packEntryLenList :: Natural -> (Natural, B.ByteString)
packEntryLenList n = let
    rest   = fromIntegral n `shiftR` 4 :: Natural
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
