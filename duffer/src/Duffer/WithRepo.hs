module Duffer.WithRepo
    ( WithRepo
    , ReaderT
    , ask
    , asks
    , local
    , runReaderT
    , liftIO
    , localObjects
    , withRepo
    ) where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.IO.Class     (liftIO)
import System.FilePath            ((</>))

import Duffer.Loose.Objects       (Repo)

type WithRepo = ReaderT Repo IO

withRepo :: Repo -> WithRepo a -> IO a
withRepo = flip runReaderT

localObjects :: WithRepo a -> WithRepo a
localObjects = local (</> "objects")
