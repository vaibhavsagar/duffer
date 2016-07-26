module Duffer.Loose where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Codec.Compression.Zlib as Z (compress, decompress)

import Control.Monad              (unless)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Data.Attoparsec.ByteString (parseOnly)
import System.Directory           (doesFileExist, createDirectoryIfMissing)
import System.FilePath            ((</>), takeDirectory)

import Duffer.Types  (GitObject(..), Ref, Repo, sha1Path, hash, showObject)
import Duffer.Parser (parseObject)

type WithRepo = ReaderT Repo IO

(~~) :: GitObject -> Int -> WithRepo GitObject
(~~) object 0 = return object
(~~) object n = readObject (head $ parentRefs object) >>= \p -> p ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = readObject $ parentRefs object !! (n-1)

compress, decompress :: B.ByteString -> B.ByteString
compress   = L.toStrict . Z.compress   . L.fromStrict
decompress = L.toStrict . Z.decompress . L.fromStrict

readObject :: Ref -> WithRepo GitObject
readObject ref = do
    path    <- asks (sha1Path ref)
    content <- liftIO $ decompress <$> B.readFile path
    return  $ either error id $ parseOnly parseObject content

writeObject :: GitObject -> WithRepo Ref
writeObject object = let sha1 = hash object in do
    path   <- asks (sha1Path sha1)
    exists <- liftIO $ doesFileExist path
    liftIO $ unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        B.writeFile path $ (compress . showObject) object
    return sha1

resolveRef :: String -> WithRepo GitObject
resolveRef = (ask >>=) . (((readObject =<<) . liftIO .
    (B.init <$>) . B.readFile) .) . flip (</>)

updateRef :: String -> GitObject -> WithRepo Ref
updateRef refPath object = asks (</> refPath) >>= liftIO . (>> return sha1) .
    flip B.writeFile (B.append sha1 "\n") where sha1 = hash object
