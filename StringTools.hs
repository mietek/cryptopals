module StringTools where

import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools
import Tools


fromHexString :: String -> String
fromHexString [] = []
fromHexString s = fromHexPair s1 ++ fromHexString s2
  where
    (s1, s2) = splitAt 2 s

toHexString :: String -> String
toHexString [] = []
toHexString s = toHexPair s1 ++ toHexString s2
  where
    (s1, s2) = splitAt 1 s


fromB64String :: String -> String
fromB64String [] = []
fromB64String s = fromB64Quad s1 ++ fromB64String s2
  where
    (s1, s2) = splitAt 4 s

toB64String :: String -> String
toB64String [] = []
toB64String s = toB64Quad s1 ++ toB64String s2
  where
    (s1, s2) = splitAt 3 s


xorString :: String -> String -> String
xorString key s = zipWith xorChar (cycle key) s


hammingDistanceString :: String -> String -> Double
hammingDistanceString s1 s2 = average (zipWith hammingDistanceChar s1 s2)


scoreWord :: String -> Double
scoreWord s = realToFrac (length (filter isLetter s)) / realToFrac (length s)

scorePhrase :: String -> Double
scorePhrase s = average (map scoreWord (words s))


crackSingleCharXor :: String -> Maybe (String, String)
crackSingleCharXor s = listToMaybe (crackSingleCharXorList s)

crackSingleCharXorList :: String -> [(String, String)]
crackSingleCharXorList s = reverse (sortBy (compare `on` scorePhrase . fst) results)
  where
    results = [(text, key) |
      k <- ['\NUL' .. '\DEL'],
      let key = [k],
      let text = xorString key s,
      all isPrint text]


crackMultipleCharXor :: String -> Maybe (String, String)
crackMultipleCharXor s = listToMaybe (crackMultipleCharXorList s)

crackMultipleCharXorList :: String -> [(String, String)]
crackMultipleCharXorList s = [(xorString key s, key) | key <- keys]
  where
    maxKeySize = min 40 (length s)
    blocks = [splitInto keySize s | keySize <- [1 .. maxKeySize]]
    sortedBlocks = sortBy (compare `on` distance) blocks
    distance [] = 1.0
    distance (b : bs) = average (map (hammingDistanceString b) bs)
    maybeResults = [sequence (map crackSingleCharXor (transpose bs)) | bs <- sortedBlocks]
    keys = map (concatMap snd) (catMaybes maybeResults)
