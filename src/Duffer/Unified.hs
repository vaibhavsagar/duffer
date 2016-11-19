module Duffer.Unified where

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative ((<|>))

import Duffer.Loose
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack
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

resolveLooseRef' :: FilePath -> WithRepo (Maybe GitObject)
resolveLooseRef' refPath = readLooseRef refPath >>=
    maybe (return Nothing) readObject

resolvePackRef' :: FilePath -> WithRepo (Maybe GitObject)
resolvePackRef' refPath = readPackRef refPath >>=
    maybe (return Nothing) readObject
