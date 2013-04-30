module ByteStringTools where

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (chr)
import Data.List (mapAccumL, tails)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools (fromB64Quad, fromHexPair, hammingDistanceChar, isLetter, isPrint, toB64Quad, toHexPair, xorChar)
import Tools (average, orderDescendingOn)


splitInto :: Int -> ByteString -> [ByteString]
splitInto n s0
  | n >= 1 = loop s0
  | otherwise = error ("splitInto: invalid n " ++ show n)
  where
    loop s
      | BS.null s = []
      | otherwise = s1 : loop s2
        where
          (s1, s2) = BS.splitAt n s

concatMapInto :: (ByteString -> ByteString) -> Int -> ByteString -> ByteString
concatMapInto f n s
  | n >= 1 = BS.concat (map f (splitInto n s))
  | otherwise = error ("concatMapInto: invalid n " ++ show n)

concatMapAccumLInto :: (a -> ByteString -> (a, ByteString)) -> a -> Int -> ByteString -> (a, ByteString)
concatMapAccumLInto f a n s
  | n >= 1 = second BS.concat (mapAccumL f a (splitInto n s))
  | otherwise = error ("concatMapAccumLInto: invalid n " ++ show n)


viaBS :: ([Char] -> [Char]) -> ByteString -> ByteString
viaBS f = BS.pack . f . BS.unpack


toHex :: ByteString -> ByteString
toHex s = concatMapInto (viaBS toHexPair) 1 s

fromHex :: ByteString -> ByteString
fromHex s = concatMapInto (viaBS fromHexPair) 2 s


toB64 :: ByteString -> ByteString
toB64 s = concatMapInto (viaBS toB64Quad) 3 s

fromB64 :: ByteString -> ByteString
fromB64 s = concatMapInto (viaBS fromB64Quad) 4 s


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


crackNXor :: Int -> ByteString -> Maybe (ByteString, ByteString)
crackNXor keySize s
  | keySize >= 1 = do
      partials <- sequence (map crack1Xor (BS.transpose blocks))
      let key = BS.concat (map snd partials)
      let text = key `xor` s
      return (text, key)
  | otherwise = error ("crackNXor: invalid key size " ++ show keySize)
  where
    blocks = splitInto keySize s


crackXor :: Int -> ByteString -> Maybe (ByteString, ByteString)
crackXor maxKeySize s = listToMaybe (crackXor' maxKeySize s)

crackXor' :: Int -> ByteString -> [(ByteString, ByteString)]
crackXor' maxKeySize s = catMaybes (map (flip crackNXor s) keySizes)
  where
    keySizes = orderDescendingOn scoreKeySize [1 .. maxKeySize]
    scoreKeySize keySize =
        case splitInto keySize s of
          [] -> 1.0
          (block : blocks) -> average (map (hammingDistance block) blocks)


detectECB :: Int -> ByteString -> Bool
detectECB blockSize s = or (map headInTail (tails (splitInto blockSize s)))
  where
    headInTail [] = False
    headInTail (block : blocks) = block `elem` blocks


padPKCS7 :: Int -> ByteString -> ByteString
padPKCS7 blockSize s
  | blockSize >= 1 || blockSize <= 256 = BS.append s (BS.replicate padSize (chr padSize))
  | otherwise = error ("padPKCS7: invalid block size " ++ show blockSize)
  where
    padSize = blockSize - (BS.length s `mod` blockSize)
