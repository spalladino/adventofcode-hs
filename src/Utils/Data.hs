module Utils.Data 
  ( assocIn,
    intersection,
    minBy
  ) where

assocIn :: Int -> a -> [a] -> [a]
assocIn i x xs = before ++ (x : after)
  where before = take i xs
        after = drop (i+1) xs

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, elem x ys]

minBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minBy f = snd . minimum . map (\x -> (f x, x))