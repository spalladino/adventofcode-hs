module Utils.Data 
  ( assocIn
  ) where

assocIn :: Int -> a -> [a] -> [a]
assocIn i x xs = before ++ (x : after)
  where before = take i xs
        after = drop (i+1) xs
