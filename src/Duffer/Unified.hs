module Duffer.Unified where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Reader (ask)

import Duffer.Loose
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack

readPackedObject :: Ref -> WithRepo (Maybe GitObject)
readPackedObject ref = do
    path <- ask
    liftIO $ readPacked ref path

readObject :: Ref -> WithRepo (Maybe GitObject)
readObject ref = do
    existsLoose <- hasLooseObject ref
    if existsLoose
        then readLooseObject  ref
        else readPackedObject ref

writeObject :: GitObject -> WithRepo Ref
writeObject = writeLooseObject
