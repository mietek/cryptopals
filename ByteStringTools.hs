module ByteStringTools where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (chr)
import Data.List (tails)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools (fromB64Quad, fromHexPair, hammingDistanceChar, isLetter, isPrint, toB64Quad, toHexPair, xorChar)
import Tools (average, orderDescendingOn)


splitInto :: Int -> ByteString -> [ByteString]
splitInto n s0 = loop s0
  where
    loop s
      | BS.null s = []
      | otherwise = s1 : loop s2
        where
          (s1, s2) = BS.splitAt n s

concatMapInto :: ([Char] -> [Char]) -> Int -> ByteString -> ByteString
concatMapInto f n s = BS.concat (map (BS.pack . f . BS.unpack) (splitInto n s))


toHex :: ByteString -> ByteString
toHex s = concatMapInto toHexPair 1 s

fromHex :: ByteString -> ByteString
fromHex s = concatMapInto fromHexPair 2 s


toB64 :: ByteString -> ByteString
toB64 s = concatMapInto toB64Quad 3 s

fromB64 :: ByteString -> ByteString
fromB64 s = concatMapInto fromB64Quad 4 s


xor :: ByteString -> ByteString -> ByteString
xor s1 s2
  | BS.length s1 <= BS.length s2 = xor' s1' s2'
  | otherwise = xor' s2' s1'
  where
    xor' key s = BS.pack (LBS.zipWith xorChar (LBS.cycle key) s)
    s1' = LBS.fromChunks [s1]
    s2' = LBS.fromChunks [s2]


hammingDistance :: ByteString -> ByteString -> Double
hammingDistance s1 s2 = average (BS.zipWith hammingDistanceChar s1 s2)


scoreWord :: ByteString -> Double
scoreWord s = realToFrac (BS.length (BS.filter isLetter s)) / realToFrac (BS.length s)

scorePhrase :: ByteString -> Double
scorePhrase s = average (map scoreWord (BS.words s))


crack1Xor :: ByteString -> Maybe (ByteString, ByteString)
crack1Xor s = listToMaybe (crack1Xor' s)

crack1Xor' :: ByteString -> [(ByteString, ByteString)]
crack1Xor' s = orderDescendingOn (scorePhrase . fst) results
  where
    keys = map BS.singleton ['\NUL' .. '\DEL']
    results = [(text, key) |
        key <- keys,
        let text = key `xor` s,
        BS.all isPrint text]


crackNXor :: ByteString -> Int -> Maybe (ByteString, ByteString)
crackNXor s keySize = do
    partials <- sequence (map crack1Xor (BS.transpose blocks))
    let key = BS.concat (map snd partials)
    let text = key `xor` s
    return (text, key)
  where
    blocks = splitInto keySize s


crackXor :: ByteString -> Maybe (ByteString, ByteString)
crackXor s = listToMaybe (crackXor' s)

crackXor' :: ByteString -> [(ByteString, ByteString)]
crackXor' s = catMaybes (map (crackNXor s) keySizes)
  where
    maxKeySize = min 40 (BS.length s)
    keySizes = orderDescendingOn scoreKeySize [1 .. maxKeySize]
    scoreKeySize keySize =
        case splitInto keySize s of
          [] -> 1.0
          (block : blocks) -> average (map (hammingDistance block) blocks)


detectECB :: ByteString -> Bool
detectECB s = or (map headInTail (tails (splitInto 16 s)))
  where
    headInTail [] = False
    headInTail (block : blocks) = block `elem` blocks


padPKCS7 :: ByteString -> Int -> ByteString
padPKCS7 s blockSize
  | blockSize <= 256 = BS.append s (BS.replicate padSize (chr padSize))
  | otherwise = error ("padPKCS7: invalid block size " ++ show blockSize)
  where
    padSize = blockSize - (BS.length s `mod` blockSize)
