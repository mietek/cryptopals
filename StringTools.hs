module StringTools where

import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools
import Tools


toHexString :: String -> String
toHexString s = concatMapInto toHexPair 1 s

fromHexString :: String -> String
fromHexString s = concatMapInto fromHexPair 2 s


toB64String :: String -> String
toB64String s = concatMapInto toB64Quad 3 s

fromB64String :: String -> String
fromB64String s = concatMapInto fromB64Quad 4 s


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
