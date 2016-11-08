{-# LANGUAGE LambdaCase #-}

module Duffer.Pack.Streaming where

import Control.Arrow (first)
import Control.Monad.Trans.State.Strict
import Data.ByteString.Base16 (decode)
import Data.Maybe (fromJust)
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

indexPackFile :: FilePath -> IO SeparatedEntries
indexPackFile path = do
    handle                    <- PB.fromHandle <$> SI.openFile path SI.ReadMode
    ((start, count), entries) <- runStateT parsePackFileStart handle
    fst <$> loopEntries entries start count emptySeparated
    where parsePackFileStart = do
            (Right (len, count)) <- fromJust <$> PA.parseL parsePackFileHeader
            return (len, count)

loopEntries
    :: Producer B.ByteString IO a -- remaining packfile input
    -> Int                        -- number of bytes read so far
    -> Int                        -- number of entries remaining
    -> SeparatedEntries           -- map of offsets to bytestrings
    -> IO (SeparatedEntries, Producer B.ByteString IO a)
loopEntries producer _      0         indexedMap = return (indexedMap, producer)
loopEntries producer offset remaining indexedMap = do
    (header, ref, decompressedP, level) <- evalStateT getNextEntry producer
    step   <- next decompressedP
    let (decompressed, eitherP) = either ((,) "" . return) id step
    (output, producer') <- advanceToCompletion decompressed eitherP
    let content     = B.concat [header, ref, compressToLevel level output]
    let indexedMap' = M.insert offset content indexedMap
    let offset'     = offset + B.length content
    let remaining'  = remaining - 1
    loopEntries producer' offset' remaining' indexedMap'
    where
        getNextEntry = do
            (Right tLen) <- fromJust <$> PA.parse parseTypeLen
            baseRef <- case fst tLen of
                OfsDeltaObject -> do
                    (Right offset) <- fromJust <$> PA.parse parseOffset
                    return $ encodeOffset offset
                RefDeltaObject -> do
                    (Right ref)    <- fromJust <$> PA.parse parseBinRef
                    return $ fst $ decode ref
                _              -> return ""
            remainder <- get
            let decompressed = PZ.decompress' PZ.defaultWindowBits remainder
            PB.drawByte
            levelByte <- fromJust <$> PB.peekByte
            let level = getCompressionLevel levelByte
            return (uncurry encodeTypeLen tLen, baseRef, decompressed, level)
        advanceToCompletion decompressed producer = next producer >>= \case
            (Left (Left p)) -> return (decompressed, p)
            (Right (d, p')) ->
                first (B.append decompressed) <$> advanceToCompletion d p'
            _               -> error "No idea how to handle Left (Right _)"
