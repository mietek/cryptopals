module Tools where


splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = xs1 : splitInto n xs2
  where
    (xs1, xs2) = splitAt n xs


average :: Fractional a => [a] -> a
average xs = sum xs / realToFrac (length xs)
