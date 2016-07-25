module Duffer.Loose where

import qualified Data.ByteString.Lazy as L

import Codec.Compression.Zlib          (compress, decompress)
import Control.Monad                   (unless)
import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Trans.Reader      (ReaderT, ask, asks)
import Data.Attoparsec.ByteString.Lazy (eitherResult, parse)
import System.Directory                (doesFileExist
                                       ,createDirectoryIfMissing)
import System.FilePath                 ((</>), takeDirectory)

import Duffer.Types
import Duffer.Parser

type WithRepo = ReaderT Repo IO

(~~) :: GitObject -> Int -> WithRepo GitObject
(~~) object 0 = return object
(~~) object n = readObject (head $ parentRefs object) >>= \p -> p ~~ (n-1)

(^^) :: GitObject -> Int -> WithRepo GitObject
(^^) object n = readObject $ parentRefs object !! (n-1)

readObject :: Ref -> WithRepo GitObject
readObject = (ask >>=) . ((liftIO . parseInflated).) . sha1Path
    where parseInflated = fmap (parseObject' . decompress) . L.readFile
          parseObject' = either error id . eitherResult . parse parseObject

writeObject :: GitObject -> WithRepo Ref
writeObject object = asks (sha1Path sha1) >>= \path ->
    liftIO $ doesFileExist path >>= flip unless (
        createDirectoryIfMissing True (takeDirectory path) >>
        L.writeFile path ((compress . showObject) object)) >>
    return sha1 where sha1 = hash object

resolveRef :: String -> WithRepo GitObject
resolveRef = (ask >>=) . (((readObject =<<) . liftIO .
    (L.init <$>) . L.readFile) .) . flip (</>)

updateRef :: String -> GitObject -> WithRepo Ref
updateRef refPath object = asks (</> refPath) >>= liftIO . (>> return sha1) .
    flip L.writeFile (L.append sha1 "\n") where sha1 = hash object
