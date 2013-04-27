module CharTools where

import Data.Bits (testBit, xor)
import Data.Char (chr, ord)


isPrint :: Char -> Bool
isPrint c =
    c == '\t' ||
    c == '\n' ||
    c >= ' ' && c <= '~'


isLetter :: Char -> Bool
isLetter c =
    c >= 'a' && c <= 'z' ||
    c >= 'A' && c <= 'Z'


isHexDigit :: Char -> Bool
isHexDigit c =
    c >= '0' && c <= '9' ||
    c >= 'a' && c <= 'f' ||
    c >= 'A' && c <= 'F'

fromHexDigit :: Char -> Int
fromHexDigit c
  | c >= '0' && c <= '9' = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error ("fromHexDigit: invalid digit " ++ show c)


isHexNumber :: Int -> Bool
isHexNumber n = n >= 0 && n <= 15

toHexDigit :: Int -> Char
toHexDigit n
  | n >= 0 && n <= 9 = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' - 10 + n)
  | otherwise = error ("toHexDigit: invalid number " ++ show n)


isB64Digit :: Char -> Bool
isB64Digit c =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '+' ||
    c == '/'

fromB64Digit :: Char -> Int
fromB64Digit c
  | c >= 'A' && c <= 'Z' = ord c - ord 'A'
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 26
  | c >= '0' && c <= '9' = ord c - ord '0' + 52
  | c == '+' = 62
  | c == '/' = 63
  | otherwise = error ("fromB64Digit: invalid digit " ++ show c)


isB64Number :: Int -> Bool
isB64Number n = n >= 0 && n <= 63

toB64Digit :: Int -> Char
toB64Digit n
  | n >= 0 && n <= 25 = chr (ord 'A' + n)
  | n >= 26 && n <= 51 = chr (ord 'a' - 26 + n)
  | n >= 52 && n <= 61 = chr (ord '0' - 52 + n)
  | n == 62 = '+'
  | n == 63 = '/'
  | otherwise = error ("toB64Digit: invalid number " ++ show n)


xorChar :: Char -> Char -> Char
xorChar k c = chr (ord k `xor` ord c)


hammingDistanceChar :: Char -> Char -> Double
hammingDistanceChar c1 c2 = realToFrac (sum (map (fromEnum . testBit x) [0 .. 7])) / 8
  where
    x = ord c1 `xor` ord c2
