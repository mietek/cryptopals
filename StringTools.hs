module StringTools where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools
import Tools


fromHexPair :: Char -> Char -> Char
fromHexPair c1 c2 = chr n
  where
    n = (k1 `shiftL` 4) .|. k2
    [k1, k2] = map fromHexDigit [c1, c2]

fromHexString :: String -> String
fromHexString [] = []
fromHexString [c1] = error ("fromHexString: truncated digits " ++ show [c1])
fromHexString (c1 : c2 : cs) = fromHexPair c1 c2 : fromHexString cs


toHexPair :: Char -> String
toHexPair c = map toHexDigit [k1, k2]
  where
    n = ord c
    k1 = (n .&. 0xF0) `shiftR` 4
    k2 = n .&. 0x0F

toHexString :: String -> String
toHexString [] = []
toHexString (c : cs) = toHexPair c ++ toHexString cs


fromB64Quad :: Char -> Char -> Char -> Char -> String
fromB64Quad c1 c2 c3 c4 = map chr [n1, n2, n3]
  where
    n1 = (k1 `shiftL` 2) .|. ((k2 .&. 0x30) `shiftR` 4)
    n2 = ((k2 .&. 0x0F) `shiftL` 4) .|. ((k3 .&. 0x3C) `shiftR` 2)
    n3 = ((k3 .&. 0x03) `shiftL` 6) .|. k4
    [k1, k2, k3, k4] = map fromB64Digit [c1, c2, c3, c4]

fromB64String :: String -> String
fromB64String [] = []
fromB64String [c1] = error ("fromB64String: truncated digits " ++ show [c1])
fromB64String [c1, c2] = take 1 (fromB64Quad c1 c2 '=' '=')
fromB64String [c1, c2, '=', '='] = take 1 (fromB64Quad c1 c2 '=' '=')
fromB64String [c1, c2, c3] = take 2 (fromB64Quad c1 c2 c3 '=')
fromB64String [c1, c2, c3, '='] = take 2 (fromB64Quad c1 c2 c3 '=')
fromB64String (c1 : c2 : c3 : c4 : cs) = fromB64Quad c1 c2 c3 c4 ++ fromB64String cs


toB64Quad :: Char -> Char -> Char -> String
toB64Quad c1 c2 c3 = map toB64Digit [k1, k2, k3, k4]
  where
    k1 = (n1 .&. 0xFC) `shiftR` 2
    k2 = ((n1 .&. 0x03) `shiftL` 4) .|. ((n2 .&. 0xF0) `shiftR` 4)
    k3 = ((n2 .&. 0x0F) `shiftL` 2) .|. ((n3 .&. 0xC0) `shiftR` 6)
    k4 = n3 .&. 0x3F
    [n1, n2, n3] = map ord [c1, c2, c3]

toB64String :: String -> String
toB64String [] = []
toB64String [c1] = take 2 (toB64Quad c1 '\NUL' '\NUL') ++ "=="
toB64String [c1, c2] = take 3 (toB64Quad c1 c2 '\NUL') ++ "="
toB64String (c1 : c2 : c3 : cs) = toB64Quad c1 c2 c3 ++ toB64String cs


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
