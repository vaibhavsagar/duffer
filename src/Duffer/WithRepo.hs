module Duffer.WithRepo
    ( WithRepo
    , ask
    , asks
    , liftIO
    , local
    , withRepo
    ) where

import Control.Monad.Trans.Reader (ask, asks, local, runReaderT, ReaderT)
import Control.Monad.IO.Class     (liftIO)

import Duffer.Loose.Objects       (Repo)

type WithRepo = ReaderT Repo IO

withRepo :: Repo -> WithRepo a -> IO a
withRepo = flip runReaderT
