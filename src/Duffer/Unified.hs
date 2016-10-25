module Duffer.Unified where

import Duffer.Loose
import Duffer.Loose.Objects (Ref, GitObject)
import Duffer.Pack
import Duffer.WithRepo

readObject :: Ref -> WithRepo (Maybe GitObject)
readObject ref = do
    existsLoose <- hasLooseObject ref
    if existsLoose
        then readLooseObject ref
        else readPackObject  ref

writeObject :: GitObject -> WithRepo Ref
writeObject = writeLooseObject
