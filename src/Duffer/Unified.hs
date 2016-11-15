module Duffer.Unified where

import Data.Bool (bool)

import Duffer.Loose
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack
import Duffer.WithRepo

readObject :: Ref -> WithRepo (Maybe GitObject)
readObject ref = hasLooseObject ref >>= bool
    (readPackObject  ref)
    (readLooseObject ref)

writeObject :: GitObject -> WithRepo Ref
writeObject = writeLooseObject

resolveRef :: FilePath -> WithRepo (Maybe GitObject)
resolveRef refPath = hasLooseRef refPath >>= bool
    (resolveLooseRef refPath)
    (resolvePackRef  refPath)
