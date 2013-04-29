module Tools where

import Data.Function (on)
import Data.List (sortBy)


isHexNumber :: (Num a, Ord a) => a -> Bool
isHexNumber n = n >= 0 && n <= 15

isB64Number :: (Num a, Ord a) => a -> Bool
isB64Number n = n >= 0 && n <= 63


splitInto :: Int -> [a] -> [[a]]
splitInto n xs0 = loop xs0
  where
    loop [] = []
    loop xs = xs1 : loop xs2
      where
        (xs1, xs2) = splitAt n xs

concatMapInto :: ([a] -> [a]) -> Int -> [a] -> [a]
concatMapInto f n xs = concatMap f (splitInto n xs)


average :: Fractional a => [a] -> a
average xs = sum xs / realToFrac (length xs)


orderDescendingOn :: Ord b => (a -> b) -> [a] -> [a]
orderDescendingOn f xs = reverse (sortBy (compare `on` f) xs)
