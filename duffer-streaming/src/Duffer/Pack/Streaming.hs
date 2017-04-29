{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Duffer.Pack.Streaming (separatePackFile) where

import Data.Attoparsec.ByteString       (takeByteString)
import Data.ByteString                  (ByteString, append, length)
import Codec.Compression.Zlib           (CompressionLevel)
import Control.Arrow                    (first)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT, gets)
import Control.Monad.Trans.Class        (lift)
import Data.ByteString.Base16           (decode)
import Data.Maybe                       (fromJust)
import Pipes                            (Producer, next, yield)
import Pipes.Attoparsec                 (parse, parseL)
import Pipes.ByteString                 (fromHandle, drawByte, peekByte)
import Pipes.Zlib                       (decompress', defaultWindowBits)
import Prelude hiding                   (length, concat)
import System.IO                        (openFile, IOMode(ReadMode))

import Duffer.Loose.Parser (parseBinRef)
import Duffer.Pack.Parser  (parseOffset, parsePackFileHeader, parseTypeLen
                           ,parsePackRegion', parsedOnly)
import Duffer.Pack.Entries (PackObjectType(..), DeltaObjectType(..), WCL(..)
                           ,PackEntry(..), compressToLevel, encodeOffset
                           ,getCompressionLevel, encodeTypeLen)

type Prod      = Producer ByteString IO ()
type ProdE a b = Producer ByteString IO (Either a b)

separatePackFile :: FilePath -> Producer (Int, PackEntry) IO ()
separatePackFile path = do
    ((start, count), entries) <- lift $ runStateT parsePackFileStart =<<
        fromHandle <$> openFile path ReadMode
    loop entries start count
    where parsePackFileStart = fromRightJust <$> parseL parsePackFileHeader

loop :: Prod -> Int -> Int -> Producer (Int, PackEntry) IO ()
loop _remains _end   0         = return ()
loop producer offset remaining = do
    (headerRef, level, output, producer') <- lift $ do
        (decompressedP, (headerRef, level)) <- evalStateT getNextEntry producer
        (output, producer')                 <- uncurry advanceToCompletion =<<
            either ((,) "" . return) id <$> next decompressedP
        return (headerRef, level, output, producer')
    let len        = length $ headerRef `append` compressToLevel level output
        parser     = parsePackRegion' (WCL level <$> takeByteString)
        entry      = parsedOnly parser $ headerRef `append` output
        offset'    = offset + len
        remaining' = remaining - 1
    yield (offset, entry)
    loop producer' offset' remaining'

getNextEntry :: StateT Prod IO (ProdE Prod (), (ByteString, CompressionLevel))
getNextEntry = do
    tLen    <- parse' id parseTypeLen
    baseRef <- case fst tLen of
        DeltaType OfsDeltaType -> parse'  encodeOffset  parseOffset
        DeltaType RefDeltaType -> parse' (fst . decode) parseBinRef
        _fullType              -> return ""
    decompressed <- gets (decompress' defaultWindowBits)
    _firstByte   <- drawByte -- The compression level is in the second byte.
    level        <- getCompressionLevel . fromJust <$> peekByte
    let headerRef = append (uncurry encodeTypeLen tLen) baseRef
    return (decompressed, (headerRef, level))
    where parse' convert = fmap (convert . fromRightJust) . parse

advanceToCompletion :: ByteString -> ProdE a b -> IO (ByteString, a)
advanceToCompletion decompressed producer = next producer >>= \case
    Right (d, p')  -> first (append decompressed) <$> advanceToCompletion d p'
    Left (Left p)  -> return (decompressed, p)
    Left (Right _) -> error "end of stream"

ifLeft :: (a -> b) -> Either a b -> b
ifLeft = flip either id

fromRightJust :: Maybe (Either a b) -> b
fromRightJust = ifLeft (const $ error "Found Left, Right expected") . fromJust
