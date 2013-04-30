module Tools where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (mapAccumL, sortBy)


isHexNumber :: (Num a, Ord a) => a -> Bool
isHexNumber n = n >= 0 && n <= 15

isB64Number :: (Num a, Ord a) => a -> Bool
isB64Number n = n >= 0 && n <= 63


splitInto :: Int -> [a] -> [[a]]
splitInto n xs0
  | n >= 1 = loop xs0
  | otherwise = error ("splitInto: invalid n " ++ show n)
  where
    loop [] = []
    loop xs = xs1 : loop xs2
      where
        (xs1, xs2) = splitAt n xs

concatMapInto :: ([a] -> [a]) -> Int -> [a] -> [a]
concatMapInto f n xs
  | n >= 1 = concatMap f (splitInto n xs)
  | otherwise = error ("concatMapInto: invalid n " ++ show n)

concatMapAccumLInto :: (a -> [b] -> (a, [b])) -> a -> Int -> [b] -> (a, [b])
concatMapAccumLInto f a n xs
  | n >= 1 = second concat (mapAccumL f a (splitInto n xs))
  | otherwise = error ("concatMapAccumLInto: invalid n " ++ show n)


average :: Fractional a => [a] -> a
average xs = sum xs / realToFrac (length xs)


orderDescendingOn :: Ord b => (a -> b) -> [a] -> [a]
orderDescendingOn f xs = reverse (sortBy (compare `on` f) xs)
