{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Crypto.Cipher.AES (initKey, decryptECB)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion)

import ByteStringTools


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
case_4 = head (reverse (sortBy (compare `on` scorePhrase . fst) partials)) @?= result
  where
    partials = catMaybes (map crack1Xor ss)
    ss = map fromHex (BS.lines (unsafePerformIO (BS.readFile "case_4.txt")))
    result = ("Now that the party is jumping\n", "5")

case_5 :: Assertion
case_5 = toHex (key `xor` s) @?= result
  where
    key = "ICE"
    s = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    result = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

case_6 :: Assertion
case_6 = snd (fromJust (crackXor s)) @?= key
  where
    s = fromB64 (BS.concat (BS.lines (unsafePerformIO (BS.readFile "case_6.txt"))))
    key = "Terminator X: Bring the noise"

case_7 :: Assertion
case_7 = last (BS.lines (decryptECB key s)) @?= lastLineText
  where
    key = initKey "YELLOW SUBMARINE"
    s = fromB64 (BS.concat (BS.lines (unsafePerformIO (BS.readFile "case_7.txt"))))
    lastLineText = "\EOT\EOT\EOT\EOT"

case_8 :: Assertion
case_8 = head (filter detectECB ss) @?= result
  where
    ss = map fromHex (BS.lines (unsafePerformIO (BS.readFile "case_8.txt")))
    result = "\216\128a\151@\168\161\155x@\168\163\FS\129\n=\bd\154\247\r\192oO\213\210\214\156tL\210\131\226\221\ENQ/kd\GS\191\157\DC1\176\&4\133B\187W\bd\154\247\r\192oO\213\210\214\156tL\210\131\148u\201\223\219\193\212e\151\148\157\156~\130\191Z\bd\154\247\r\192oO\213\210\214\156tL\210\131\151\169>\171\141j\236\213fH\145Tx\154k\ETX\bd\154\247\r\192oO\213\210\214\156tL\210\131\212\ETX\CAN\f\152\200\246\219\US*?\156@@\222\176\171Q\178\153\&3\242\193#\197\131\134\176o\186\CANj"


main :: IO ()
main = $(defaultMainGenerator)
