
module Exercise6 where
import System.Random
import Test.QuickCheck
import Lecture2
import Data.Char

{-------------------------------------------------------------------------------------------------------------------------------------
a)  Implementing and testing IBAN validation

    The International Bank Account Number (IBAN) was designed to facility international money transfer, to uniquely identify bank accounts worldwide.
    It is described here, including a procedure for validating IBAN codes. Write a function

    iban :: String -> Bool
    that implements this validation procedure.

    Next, test your implementation using some suitable list of examples.

    Note It is not enough to test only with correct examples. You should invent a way to test with incorrect examples also.
--------------------------------------------------------------------------------------------------------------------------------------}

-- source https://en.wikipedia.org/wiki/International_Bank_Account_Number

{--
    Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
    Move the four initial characters to the end of the string
    Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
    Interpret the string as a decimal integer and compute the remainder of that number on division by 97
--}
iban :: String -> Bool
iban num = hasCorrectLength num &&
           hasCorrectPrefix num &&
           isIbanAlphaNum num &&
           joiner (replaceCharWithInt $ rearrange num) `mod` 97 == 1


-- First check is if the length of the iban is below 35
hasCorrectLength :: String -> Bool
hasCorrectLength x = length x <= 34

isIbanAlphaNum :: [Char] -> Bool
isIbanAlphaNum xs = forall xs (\ x -> isAlphaNum x) -- Source is Data.Char

hasCorrectPrefix :: String -> Bool
hasCorrectPrefix xs = countryCode `elem` validCountryCodes
  where countryCode = take 2 xs

validCountryCodes :: [String]
validCountryCodes = ["NL" , "BE"]

rearrange :: [Char] -> [Char]
rearrange xs = drop 4 xs ++ (take 4 xs)

a1 :: String
a1 = "NL39RABO0300065264"

-- Replace each char with an int
replaceCharWithInt:: String -> [Int]
replaceCharWithInt xs = map digitToInt' xs

-- Convert a single char to int
-- We get the ascii value of lower case char and substract 87 to get ['a'..'z'] equal to [10..35]
-- https://stackoverflow.com/questions/3261236/how-to-get-ascii-value-of-character-in-haskell
digitToInt' :: Char -> Int
digitToInt' c | isLetter c = (ord (toLower c) - 87)
              | otherwise = digitToInt c

-- Source https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
joiner :: [Int] -> Integer
joiner = read . concatMap show

{-------------------------------------------------------------------------------------------------------------------------------------
    Can you automate the test process?
    Deliverables: Haskell program, concise test report, indication of time spent.
--------------------------------------------------------------------------------------------------------------------------------------}
