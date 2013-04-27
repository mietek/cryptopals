module ByteStringTools where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)

import CharTools
import Tools hiding (splitInto, concatMapInto)


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
xor key s = BS.pack (LBS.zipWith xorChar (LBS.cycle key') s')
  where
    key' = LBS.fromChunks [key]
    s' = LBS.fromChunks [s]


hammingDistance :: ByteString -> ByteString -> Double
hammingDistance s1 s2 = average (BS.zipWith hammingDistanceChar s1 s2)


scoreWord :: ByteString -> Double
scoreWord s = realToFrac (BS.length (BS.filter isLetter s)) / realToFrac (BS.length s)

scorePhrase :: ByteString -> Double
scorePhrase s = average (map scoreWord (BS.words s))


crack1Xor :: ByteString -> Maybe (ByteString, ByteString)
crack1Xor s = listToMaybe (crack1Xor' s)

crack1Xor' :: ByteString -> [(ByteString, ByteString)]
crack1Xor' s = reverse (sortBy (compare `on` scorePhrase . fst) results)
  where
    keys = map BS.singleton ['\NUL' .. '\DEL']
    results = [(text, key) |
        key <- keys,
        let text = key `xor` s,
        BS.all isPrint text]


crackNXor :: Int -> ByteString -> Maybe (ByteString, ByteString)
crackNXor keySize s = do
    partials <- sequence (map crack1Xor (BS.transpose blocks))
    let key = BS.concat (map snd partials)
    let text = key `xor` s
    return (text, key)
  where
    blocks = splitInto keySize s


crackXor :: ByteString -> Maybe (ByteString, ByteString)
crackXor s = listToMaybe (crackXor' s)

crackXor' :: ByteString -> [(ByteString, ByteString)]
crackXor' s = catMaybes [crackNXor keySize s | keySize <- keySizes]
  where
    maxKeySize = min 40 (BS.length s)
    keySizes = reverse (sortBy (compare `on` scoreKeySize) [1 .. maxKeySize])
    scoreKeySize keySize =
        case splitInto keySize s of
          [] -> 1.0
          (block : blocks) -> average (map (hammingDistance block) blocks)
