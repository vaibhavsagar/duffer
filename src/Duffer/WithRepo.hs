module Duffer.WithRepo
    ( WithRepo
    , ask
    , asks
    , liftIO
    ) where

import Control.Monad.Trans.Reader (ask, asks, ReaderT)
import Control.Monad.IO.Class     (liftIO)

import Duffer.Loose.Objects       (Repo)

type WithRepo = ReaderT Repo IO
