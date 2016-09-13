module Duffer.Pack.Streaming where

import Control.Monad.Trans.State.Strict
import Data.ByteString.Base16 (decode)
import qualified Data.ByteString  as B
import qualified Data.Map.Strict  as M
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Zlib       as PZ
import qualified System.IO        as SI
import Pipes
import Prelude hiding (take)

import Duffer.Loose.Parser (parseBinRef)
import Duffer.Pack.Parser
import Duffer.Pack.Entries

type SeparatedEntries = M.Map Int B.ByteString

emptySeparated :: SeparatedEntries
emptySeparated = M.empty

indexPackfile :: FilePath -> IO SeparatedEntries
indexPackfile path = do
    handle                    <- PB.fromHandle <$> SI.openFile path SI.ReadMode
    ((start, count), entries) <- runStateT parsePackfileStart handle
    fst <$> loopEntries entries start count M.empty
    where parsePackfileStart = do
            Just (Right (lenHeader, noOfObjects)) <- PA.parseL parsePackfileHeader
            return (lenHeader, noOfObjects)

loopEntries :: Producer B.ByteString IO a -> Int -> Int -> SeparatedEntries -> IO (SeparatedEntries, Producer B.ByteString IO a)
loopEntries producer offset remaining indexedMap = case remaining of
    0 -> return (indexedMap, producer)
    _ -> do
        (header, ref, decompressedP, level) <- evalStateT getNextEntry producer
        step   <- next decompressedP
        let (decompressed, eitherP) = case step of
                (Right (d, p)) -> (d,  p)
                (Left  p')     -> ("", return p')
        (decompressed', producer') <- advanceToCompletion decompressed eitherP
        let content     = B.concat [header, ref, compressToLevel level decompressed']
        let indexedMap' = M.insert offset content indexedMap
        let offset'     = offset + B.length content
        let remaining'  = remaining - 1
        loopEntries producer' offset' remaining' indexedMap'
    where advanceToCompletion decompressed producer = do
            step <- next producer
            case step of
                (Left (Left p)) -> return (decompressed, p)
                (Right (d, p')) -> do
                    (d', p) <- advanceToCompletion d p'
                    return (B.append decompressed d', p)
                _               -> error "No idea how to handle this result"
          getNextEntry = do
            Just (Right typeLen) <- PA.parse parseTypeLen
            Just (Right baseRef) <- case fst typeLen of
                OfsDeltaObject -> do
                    Just (Right offset) <- PA.parse parseOffset
                    return $ Just (Right (encodeOffset offset))
                RefDeltaObject -> do
                    Just (Right ref)    <- PA.parse parseBinRef
                    return $ Just (Right (fst $ decode ref))
                _              -> return $ Just (Right "")
            remainder <- get
            let decompressed = PZ.decompress' PZ.defaultWindowBits remainder
            PB.drawByte
            Just levelByte <- PB.peekByte
            let level = getCompressionLevel levelByte
            return (uncurry encodeTypeLen typeLen, baseRef, decompressed, level)
