module Duffer.WithRepo
    ( WithRepo
    , module Control.Monad.Trans.Reader
    , liftIO
    , localObjects
    , withRepo
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class     (liftIO)
import System.FilePath            ((</>))

import Duffer.Loose.Objects       (Repo)

type WithRepo = ReaderT Repo IO

withRepo :: Repo -> WithRepo a -> IO a
withRepo = flip runReaderT

localObjects :: WithRepo a -> WithRepo a
localObjects = local (</> "objects")
