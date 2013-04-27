module Tools where


isHexNumber :: (Num a, Ord a) => a -> Bool
isHexNumber n = n >= 0 && n <= 15

isB64Number :: (Num a, Ord a) => a -> Bool
isB64Number n = n >= 0 && n <= 63


splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = xs1 : splitInto n xs2
  where
    (xs1, xs2) = splitAt n xs


average :: Fractional a => [a] -> a
average xs = sum xs / realToFrac (length xs)
