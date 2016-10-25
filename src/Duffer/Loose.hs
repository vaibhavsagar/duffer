module Duffer.Loose where

import qualified Data.ByteString.Lazy   as L
import qualified Codec.Compression.Zlib as Z (compress, decompress)

import Control.Monad              (unless)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString            (ByteString, append, readFile, writeFile
                                  ,init)
import Data.Maybe                 (fromJust)
import System.Directory           (doesFileExist, createDirectoryIfMissing)
import System.FilePath            ((</>), takeDirectory)

import Prelude hiding (readFile, writeFile, init)

import Duffer.Loose.Objects (GitObject(..), Ref, sha1Path, hash, showObject)
import Duffer.Loose.Parser  (parseObject)
import Duffer.WithRepo      (WithRepo, asks, liftIO)

(~~) :: GitObject -> Int -> WithRepo (Maybe GitObject)
(~~) object 0 = return (Just object)
(~~) object n = do
    parent <- readLooseObject (head $ parentRefs object)
    case parent of
        Just p  -> p ~~ (n-1)
        Nothing -> return Nothing

(^^) :: GitObject -> Int -> WithRepo (Maybe GitObject)
(^^) object n = readLooseObject $ parentRefs object !! (n-1)

decompress :: ByteString -> ByteString
decompress = L.toStrict . Z.decompress . L.fromStrict

readLooseObject :: Ref -> WithRepo (Maybe GitObject)
readLooseObject ref = do
    exists  <- hasLooseObject ref
    if exists
        then do
            path       <- asks (sha1Path ref)
            content    <- liftIO $ decompress <$> readFile path
            let parsed =  parseOnly parseObject content
            return $ either (const Nothing) Just parsed
        else return Nothing

writeLooseObject :: GitObject -> WithRepo Ref
writeLooseObject object = let sha1 = hash object in do
    path   <- asks (sha1Path sha1)
    exists <- liftIO $ doesFileExist path
    liftIO $ unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        L.writeFile path $ (Z.compress . showObject) object
    return sha1

hasLooseObject :: Ref -> WithRepo Bool
hasLooseObject ref = do
    path <- asks (sha1Path ref)
    liftIO $ doesFileExist path

resolveRef :: FilePath -> WithRepo GitObject
resolveRef refPath = do
    path <- asks (</> refPath)
    ref  <- liftIO $ init <$> readFile path
    fromJust <$> readLooseObject ref

updateRef :: FilePath -> GitObject -> WithRepo Ref
updateRef refPath object = do
    let sha1 =  hash object
    path     <- asks (</> refPath)
    liftIO $ writeFile path (append sha1 "\n")
    return sha1
