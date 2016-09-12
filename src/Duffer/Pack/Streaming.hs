module Duffer.Pack.Streaming where

import Control.Monad.Trans.State.Strict
import qualified Data.ByteString  as B
import qualified Data.Map.Strict  as M
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse      as PP
import qualified Pipes.Zlib       as PZ
import qualified System.IO        as SI
import Pipes
import Prelude hiding (take)

import Duffer.Pack.Parser
import Duffer.Pack.Entries

type SeparatedEntries = M.Map Int B.ByteString

emptySeparated :: SeparatedEntries
emptySeparated = M.empty

-- getNextEntry :: PP.Parser B.ByteString IO a
getNextEntry = do
    Just (Right typeLen) <- PA.parse parseTypeLen
    remainder <- get
    let decompressed = PZ.decompress' PZ.defaultWindowBits remainder
    PB.drawByte
    Just levelByte <- PB.peekByte
    return (uncurry encodeTypeLen typeLen, decompressed, levelByte)

parsePackfileStart = do
    Just (Right (lenHeader, noOfObjects)) <- PA.parseL parsePackfileHeader
    return (lenHeader, noOfObjects)

indexPackfile :: FilePath -> IO SeparatedEntries
indexPackfile path = do
    handle                    <- PB.fromHandle <$> SI.openFile path SI.ReadMode
    ((start, count), entries) <- runStateT parsePackfileStart handle
    fst <$> loopEntries entries start count M.empty


loopEntries :: Producer B.ByteString IO a -> Int -> Int -> SeparatedEntries -> IO (SeparatedEntries, Producer B.ByteString IO a)
loopEntries producer offset remaining indexedMap = case remaining of
    0 -> return (indexedMap, producer)
    _ -> do
        (header, decompressedP, levelB) <- evalStateT getNextEntry producer
        Right (decompressed, eitherP)   <- next decompressedP
        Left (Left producer')           <- next eitherP
        let level       = getCompressionLevel levelB
        let content     = B.append header $ compressToLevel level decompressed
        let indexedMap' = M.insert offset content indexedMap
        let offset'     = offset + B.length content
        let remaining'  = remaining - 1
        loopEntries producer' offset' remaining' indexedMap'

