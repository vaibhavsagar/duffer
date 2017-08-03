module Duffer.Loose (module Duffer.Loose) where

import qualified Data.ByteString.Lazy   as L (toStrict, fromStrict, writeFile)
import qualified Codec.Compression.Zlib as Z (compress, decompress)

import Control.Monad              (unless)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bool                  (bool)
import Data.ByteString            (ByteString, append, readFile, writeFile)
import System.Directory           (doesFileExist, createDirectoryIfMissing)
import System.FilePath            ((</>), takeDirectory)

import Prelude hiding (readFile, writeFile, init)

import Duffer.Loose.Objects (GitObjectGeneric(..), GitObject, Ref, sha1Path
                            ,hash, showObject, Commit(..))
import Duffer.Loose.Parser  (parseObject)
import Duffer.WithRepo      (WithRepo, asks, localObjects, liftIO)

(~~) :: GitObject -> Int -> WithRepo (Maybe GitObject)
(~~) (GitCommit object) 0 = return (Just $ GitCommit object)
(~~) (GitCommit object) n = readLooseObject (head $ commitParentRefs object)
    >>= maybe (return Nothing) (~~ (n-1))
(~~) _                  _ = error "only works with commits"

(^^) :: GitObject -> Int -> WithRepo (Maybe GitObject)
(^^) (GitCommit object) n = readLooseObject $ commitParentRefs object !! (n-1)
(^^) _                  _ = error "only works with commits"

decompress :: ByteString -> ByteString
decompress = L.toStrict . Z.decompress . L.fromStrict

readLooseObject :: Ref -> WithRepo (Maybe GitObject)
readLooseObject = localObjects . readLooseObject'

readLooseObject' :: Ref -> WithRepo (Maybe GitObject)
readLooseObject' ref = hasLooseObject' ref >>= bool
    (return Nothing)
    ((either (const Nothing) Just . parseOnly parseObject) <$>
    (asks (sha1Path ref) >>= liftIO . fmap decompress . readFile))

writeLooseObject :: GitObject -> WithRepo Ref
writeLooseObject = localObjects . writeLooseObject'

writeLooseObject' :: GitObject -> WithRepo Ref
writeLooseObject' object = let sha1 = hash object in do
    path   <- asks (sha1Path sha1)
    exists <- hasLooseObject' sha1
    liftIO . unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        L.writeFile path $ (Z.compress . L.fromStrict . showObject) object
    return sha1

hasLooseObject :: Ref -> WithRepo Bool
hasLooseObject = localObjects . hasLooseObject'

hasLooseObject' :: Ref -> WithRepo Bool
hasLooseObject' = hasFile sha1Path

hasLooseRef :: FilePath -> WithRepo Bool
hasLooseRef = hasFile (flip (</>))

hasFile :: (a -> FilePath -> FilePath) -> a -> WithRepo Bool
hasFile f path = liftIO . doesFileExist =<< asks (f path)

updateLooseRef :: FilePath -> GitObject -> WithRepo Ref
updateLooseRef refPath object = let sha1 = hash object in do
    asks (</> refPath) >>= liftIO . flip writeFile (sha1 `append` "\n")
    return sha1
