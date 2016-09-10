module Duffer.Pack.Streaming where

import Control.Monad.Trans.State.Strict
import Data.Attoparsec.ByteString
import Data.Byteable
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString        as B
import qualified Data.Map.Strict        as M
import qualified Codec.Compression.Zlib as Z
import Pipes
import Prelude hiding (take)
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as PP
import qualified Pipes.Zlib as PZ
import qualified Pipes.Attoparsec as PA
import qualified System.IO as SI

import Duffer.Pack.Parser
import Duffer.Pack.Entries
import Duffer.Loose.Parser

getNextEntry = do
    Just (Right typeLen) <- PA.parse parseTypeLen
    remainder <- get
    PB.drawByte
    Just levelByte <- PB.peekByte
    let level = getCompressionLevel levelByte
    let decompressed = PZ.decompress' PZ.defaultWindowBits remainder
    return (uncurry encodeTypeLen typeLen, decompressed, level)

parsePackfileStart = do
    Just (Right (lenHeader, noOfObjects)) <- PA.parseL parsePackfileHeader
    return (lenHeader, noOfObjects)

indexPackfile path = do
    handle                     <- PB.fromHandle <$> SI.openFile path SI.ReadMode
    ((start, count), entriesP) <- runStateT parsePackfileStart handle
    loopEntries entriesP start count M.empty

type SeparatedEntries = M.Map Int B.ByteString

loopEntries :: Producer B.ByteString IO a -> Int -> Int -> SeparatedEntries -> IO (SeparatedEntries)
loopEntries producer offset remaining indexedMap = case remaining of
    0 -> return indexedMap
    _ -> do
        ((header, decompressedP, level), _) <- runStateT getNextEntry producer
        Right (decompressed, producer')     <- next decompressedP
        let content     = B.append header $ compressToLevel level decompressed
        let indexedMap' = M.insert offset content indexedMap
        let offset'     = offset + B.length content
        let remaining'  = remaining - 1
        loopEntries producer' offset' remaining' indexedMap'

