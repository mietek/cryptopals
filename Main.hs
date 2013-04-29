{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Crypto.Cipher.AES (initKey, decryptECB)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes, fromJust)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion)

import ByteStringTools
import Tools


case_1 :: Assertion
case_1 = toB64 s @?= result
  where
    s = fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    result = toB64 "I'm killing your brain like a poisonous mushroom"

case_2 :: Assertion
case_2 = toHex (key `xor` s) @?= result
  where
    key = fromHex "1c0111001f010100061a024b53535009181c"
    s = fromHex "686974207468652062756c6c277320657965"
    result = toHex "the kid don't play"

case_3 :: Assertion
case_3 = fromJust (crack1Xor s) @?= result
  where
    s = fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    result = ("Cooking MC's like a pound of bacon", "X")

case_4 :: Assertion
case_4 = do
    ss <- fmap (map fromHex . BS.lines) (BS.readFile "case_4.txt")
    head (orderDescendingOn (scorePhrase . fst) (catMaybes (map crack1Xor ss))) @?= result
  where
    result = ("Now that the party is jumping\n", "5")

case_5 :: Assertion
case_5 = toHex (key `xor` s) @?= result
  where
    key = "ICE"
    s = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    result = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

case_6 :: Assertion
case_6 = do
    s <- fmap (fromB64 . BS.concat . BS.lines) (BS.readFile "case_6.txt")
    result <- BS.readFile "result_6.txt"
    fromJust (crackXor s) @?= (result, resultKey)
  where
    resultKey = "Terminator X: Bring the noise"

case_7 :: Assertion
case_7 = do
    s <- fmap (fromB64 . BS.concat . BS.lines) (BS.readFile "case_7.txt")
    result <- BS.readFile "result_7.txt"
    decryptECB key s @?= result
  where
    key = initKey "YELLOW SUBMARINE"

case_8 :: Assertion
case_8 = do
    ss <- fmap (map fromHex . BS.lines) (BS.readFile "case_8.txt")
    head (filter detectECB ss) @?= result
  where
    result = fromHex "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"


main :: IO ()
main = $(defaultMainGenerator)
