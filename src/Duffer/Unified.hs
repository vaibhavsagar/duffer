module Duffer.Unified where

import Control.Applicative        ((<|>))
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
    maybeLoose  <- readLooseObject  ref
    maybePacked <- readPackedObject ref
    return $ maybeLoose <|> maybePacked
