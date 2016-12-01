module Duffer.Unified where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString            (init, readFile)
import Data.Bool                  (bool)
import Control.Monad.Trans.Maybe  (MaybeT(..), runMaybeT)
import Control.Applicative        ((<|>))
import System.FilePath            ((</>))

import Prelude hiding (init, readFile)

import Duffer.Loose
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack
import Duffer.Loose.Parser  (parseSymRef)
import Duffer.WithRepo

readObject :: Ref -> WithRepo (Maybe GitObject)
readObject ref = runMaybeT $
    MaybeT (readLooseObject ref) <|> MaybeT (readPackObject ref)

writeObject :: GitObject -> WithRepo Ref
writeObject = writeLooseObject

resolveRef :: FilePath -> WithRepo (Maybe GitObject)
resolveRef refPath = readRef refPath >>= maybe (return Nothing) readObject

readRef :: FilePath -> WithRepo (Maybe Ref)
readRef refPath = runMaybeT $
    MaybeT (readLooseRef refPath) <|> MaybeT (readPackRef refPath)

readLooseRef :: FilePath -> WithRepo (Maybe Ref)
readLooseRef refPath = hasLooseRef refPath >>= bool
    (return Nothing)
    (do
        path    <- asks (</> refPath)
        content <- liftIO $ readFile path
        case parseOnly parseSymRef content of
            Right newPath -> readRef newPath
            Left _        -> return $ Just (init content))

updateRef :: FilePath -> GitObject -> WithRepo Ref
updateRef = updateLooseRef
