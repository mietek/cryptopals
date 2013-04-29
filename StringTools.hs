module StringTools where

import Data.List (tails, transpose)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools
import Tools


toHex :: String -> String
toHex s = concatMapInto toHexPair 1 s

fromHex :: String -> String
fromHex s = concatMapInto fromHexPair 2 s


toB64 :: String -> String
toB64 s = concatMapInto toB64Quad 3 s

fromB64 :: String -> String
fromB64 s = concatMapInto fromB64Quad 4 s


xor :: String -> String -> String
xor s1 s2
  | length s1 <= length s2 = xor' s1 s2
  | otherwise = xor' s2 s1
  where
    xor' key s = zipWith xorChar (cycle key) s


hammingDistance :: String -> String -> Double
hammingDistance s1 s2 = average (zipWith hammingDistanceChar s1 s2)


scoreWord :: String -> Double
scoreWord s = realToFrac (length (filter isLetter s)) / realToFrac (length s)

scorePhrase :: String -> Double
scorePhrase s = average (map scoreWord (words s))


crack1Xor :: String -> Maybe (String, String)
crack1Xor s = listToMaybe (crack1Xor' s)

crack1Xor' :: String -> [(String, String)]
crack1Xor' s = orderDescendingOn (scorePhrase . fst) results
  where
    keys = map (: []) ['\NUL' .. '\DEL']
    results = [(text, key) |
        key <- keys,
        let text = key `xor` s,
        all isPrint text]


crackNXor :: String -> Int -> Maybe (String, String)
crackNXor s keySize = do
    partials <- sequence (map crack1Xor (transpose blocks))
    let key = concatMap snd partials
    let text = key `xor` s
    return (text, key)
  where
    blocks = splitInto keySize s


crackXor :: String -> Maybe (String, String)
crackXor s = listToMaybe (crackXor' s)

crackXor' :: String -> [(String, String)]
crackXor' s = catMaybes (map (crackNXor s) keySizes)
  where
    maxKeySize = min 40 (length s)
    keySizes = orderDescendingOn scoreKeySize [1 .. maxKeySize]
    scoreKeySize keySize =
        case splitInto keySize s of
          [] -> 1.0
          (block : blocks) -> average (map (hammingDistance block) blocks)


detectECB :: String -> Bool
detectECB s = or (map headInTail (tails (splitInto 16 s)))
  where
    headInTail [] = False
    headInTail (block : blocks) = block `elem` blocks
