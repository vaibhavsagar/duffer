module Duffer.Misc where

ifLeft :: (a -> b) -> Either a b -> b
ifLeft f = either f id

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = ((.).(.))
