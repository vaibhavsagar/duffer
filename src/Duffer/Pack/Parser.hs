module Duffer.Pack.Parser where

import qualified Data.ByteString as B

import Data.Attoparsec.ByteString

import Data.ByteString.Base16 (encode)
import GHC.Word               (Word8)

import Prelude hiding (take)

import Duffer.Packed.PackIndex


word8s :: [Word8] -> Parser [Word8]
word8s = mapM word8

parsePackIndex :: Parser [PackIndexEntry]
parsePackIndex = do
    header    <- word8s [255, 116, 79, 99]
    version   <- word8s [0, 0, 0, 2]
    totals    <- count 256 (fromPack <$> take 4)
    let total =  count (last totals)
    names     <- total $ encode      <$> take 20
    checks    <- total $ take 4
    offsets   <- total $ fromPack    <$> take 4
    remaining <- takeByteString
    let (fifth, checks) = B.splitAt (B.length remaining - 40) remaining
    let fixedOffsets    = if fifth /= B.empty
        then map (fixOffsets (fifthOffsets fifth)) offsets
        else offsets
    return $ zipWith PackIndexEntry names fixedOffsets
