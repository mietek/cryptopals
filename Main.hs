{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion)

import StringTools


case_1 :: Assertion
case_1 = toB64 xs @?= result
  where
    xs = fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    result = toB64 "I'm killing your brain like a poisonous mushroom"

case_2 :: Assertion
case_2 = toHex (key `xor` xs) @?= result
  where
    key = fromHex "1c0111001f010100061a024b53535009181c"
    xs = fromHex "686974207468652062756c6c277320657965"
    result = toHex "the kid don't play"

case_3 :: Assertion
case_3 = fromJust (crack1Xor xs) @?= result
  where
    xs = fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    result = ("Cooking MC's like a pound of bacon", "X")

case_4 :: Assertion
case_4 = head (reverse (sortBy (compare `on` scorePhrase . fst) (catMaybes (map crack1Xor xss)))) @?= result
  where
    xss = map fromHex (lines (unsafePerformIO (readFile "case_4.txt")))
    result = ("Now that the party is jumping\n", "5")

case_5 :: Assertion
case_5 = toHex (key `xor` xs) @?= result
  where
    key = "ICE"
    xs = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    result = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

case_6 :: Assertion
case_6 = snd (fromJust (crackMultipleXor xs)) @?= key
  where
    xs = fromB64 (concat (lines (unsafePerformIO (readFile "case_6.txt"))))
    key = "Terminator X: Bring the noise"


main :: IO ()
main = $(defaultMainGenerator)
