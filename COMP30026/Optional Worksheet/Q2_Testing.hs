module Testing (both) where

both :: (a -> b) -> ((a, a) -> (b, b))
both f (x, y) = (f x, f y)