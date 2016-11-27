module Duffer.Loose where

import qualified Data.ByteString.Lazy   as L (toStrict, fromStrict, writeFile)
import qualified Codec.Compression.Zlib as Z (compress, decompress)

import Control.Monad              (unless)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bool                  (bool)
import Data.ByteString            (ByteString, append, readFile, writeFile
                                  ,init)
import System.Directory           (doesFileExist, createDirectoryIfMissing
                                  ,getDirectoryContents)
import System.FilePath            ((</>), takeDirectory)

import Prelude hiding (readFile, writeFile, init)

import Duffer.Loose.Objects (GitObject(..), Ref, sha1Path, hash, showObject)
import Duffer.Loose.Parser  (parseObject)
import Duffer.WithRepo      (WithRepo, asks, localObjects, liftIO)

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

listDirectory :: FilePath -> IO [FilePath]
listDirectory =
    fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

readLooseObject :: Ref -> WithRepo (Maybe GitObject)
readLooseObject = localObjects . readLooseObject'

readLooseObject' :: Ref -> WithRepo (Maybe GitObject)
readLooseObject' ref = hasLooseObject' ref >>= bool
    (return Nothing)
    ((either (const Nothing)  Just . parseOnly parseObject) <$>
    (asks (sha1Path ref) >>= liftIO . fmap decompress . readFile))

writeLooseObject :: GitObject -> WithRepo Ref
writeLooseObject = localObjects . writeLooseObject'

writeLooseObject' :: GitObject -> WithRepo Ref
writeLooseObject' object = let sha1 = hash object in do
    path   <- asks (sha1Path sha1)
    exists <- hasLooseObject' sha1
    liftIO $ unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        L.writeFile path $ (Z.compress . showObject) object
    return sha1

hasLooseObject :: Ref -> WithRepo Bool
hasLooseObject = localObjects . hasLooseObject'

hasLooseObject' :: Ref -> WithRepo Bool
hasLooseObject' ref = asks (sha1Path ref) >>= liftIO . doesFileExist

hasLooseRef :: FilePath -> WithRepo Bool
hasLooseRef refPath = asks (</> refPath) >>= liftIO . doesFileExist

updateRef :: FilePath -> GitObject -> WithRepo Ref
updateRef refPath object = do
    let sha1 =  hash object
    path     <- asks (</> refPath)
    liftIO $ writeFile path (append sha1 "\n")
    return sha1
