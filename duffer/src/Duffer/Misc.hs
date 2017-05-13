module Duffer.Misc (module Duffer.Misc) where

import Data.Functor.Compose (Compose(..))

ifLeft :: (a -> b) -> Either a b -> b
ifLeft = flip either id

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

compose :: (Compose f g a -> Compose f g b) -> f (g a) -> f (g b)
compose f = getCompose . f . Compose
