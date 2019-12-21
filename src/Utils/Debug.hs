module Utils.Debug (
  put, putf
) where

import Debug.Trace

put :: Show a => a -> a
put x = trace (show x) x

putf :: Show b => (a -> b) -> a -> a
putf f x = trace (show (f x)) x
