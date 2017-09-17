{-
Assignment:		Lab 2: Exercise 7 - Implementing and testing IBAN validation
Name:           Sangam Gupta
Time spent:     4h
Sources:        - https://en.wikipedia.org/wiki/International_Bank_Account_Number
                - https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
---------------}
module Exc7IBAN where
import System.Random
import Test.QuickCheck
import Lecture2
import Data.Char

{-------------------------------------------------------------------------------------------------------------------------------------
Exercise Implementing and testing IBAN validation

a)  Implementing and testing IBAN validation

    The International Bank Account Number (IBAN) was designed to facility international money transfer, to uniquely identify bank accounts worldwide.
    It is described here, including a procedure for validating IBAN codes. Write a function

    iban :: String -> Bool
    that implements this validation procedure.

    Next, test your implementation using some suitable list of examples.

    Note It is not enough to test only with correct examples. You should invent a way to test with incorrect examples also.
--------------------------------------------------------------------------------------------------------------------------------------}


{--
    Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
    Move the four initial characters to the end of the string
    Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
    Interpret the string as a decimal integer and compute the remainder of that number on division by 97
--}
iban :: String -> Bool
iban n = isIbanFormat n &&
           (joiner . replaceCharWithInt . rearrange $ n) `mod` 97 == 1

-- String is CountryCode and Int is the length of iban
ibanFormat :: [(String, Int)]
ibanFormat = [("NL", 18), ("BE", 16), ("HR", 21)]

-- By checking it against a tuple we can give up more specific specifications per country
isIbanFormat :: String -> Bool
isIbanFormat iban = (length . filter (\x -> fst x == take 2 iban && snd x == length iban) $ ibanFormat) == 1

rearrange :: [Char] -> [Char]
rearrange xs = drop 4 xs ++ (take 4 xs)

-- Replace each char with an int
replaceCharWithInt:: String -> [Int]
replaceCharWithInt xs = map digitToInt' xs

-- Convert a single char to int
-- We get the ascii value of lower case char and substract 87 to get ['a'..'z'] equal to [10..35]
digitToInt' :: Char -> Int
digitToInt' c | isLetter c = (ord (toLower c) - 87)
              | otherwise = digitToInt c

-- Source https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
joiner :: [Int] -> Integer
joiner = read . concatMap show

validIbanNumbers, invalidIbanNumers :: [String]
validIbanNumbers = ["NL39RABO0300065264", "BE62510007547061", "HR1210010051863000160"]
invalidIbanNumers =  ["NL49RABO0300065262", "BE67510007547063", "IT1210010051863000160"]

testValidIban :: Bool
testValidIban = forall validIbanNumbers (\x -> iban x)  -- expect all to be true

testInvalidIban :: Bool
testInvalidIban = forall invalidIbanNumers (\x -> not (iban x))  -- expect all to be false

main = do
  print "Testing valid iban numbers:"
  print testValidIban
  print "Testing invalid iban numbers:"
  print testInvalidIban


{-------------------------------------------------------------------------------------------------------------------------------------
    Can you automate the test process?

    It is possible but very difficult to automate this proces because its difficult to generate random IBAN numbers.
    There are lots of variables which you should pay attention too. For example many countries have different IBAN format.

    Deliverables: Haskell program, concise test report, indication of time spent.
--------------------------------------------------------------------------------------------------------------------------------------}
