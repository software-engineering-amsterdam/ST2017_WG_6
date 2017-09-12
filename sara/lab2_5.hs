module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2


{-
    Implementing and testing ROT13 encoding

    ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers. See also www.rot13.com.

    First, give a specification of ROT13.
----------
"Each English alphabetic letter is replaced by the letter 13 places further along in the alphabet,
cycling back to the start when passing the 26th character 'z'. The fact that there are 26 English alphabetic characters and
that 26 / 2 = 13, ROT13 inverses the letters, meaning applying it twice will result in the same string as before."
----------

    Next, give a simple implementation of ROT13.

    Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.

    2h 30m

    Source used: https://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Char.html

-}

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:cs) = (rot c) : (rot13 cs)

rot :: Char -> Char
rot c | (isLetter c) = toEnum (newCharIndex c) :: Char
      | otherwise = c


newCharIndex :: Char -> Int
newCharIndex c | (isAsciiLower c) = rotIndexMinMax (fromEnum 'a' :: Int) (fromEnum 'z' :: Int) (fromEnum c :: Int)
               | (isAsciiUpper c) = rotIndexMinMax (fromEnum 'A' :: Int) (fromEnum 'Z' :: Int) (fromEnum c :: Int)
               | otherwise = fromEnum c :: Int

rotIndexMinMax :: Int -> Int -> Int -> Int
rotIndexMinMax i j n | (13 + n) > j = i-1 + ((13 + n) - j) -- i-1 to restore the index otherwise it skips 'a'
                     | otherwise = 13 + n


rot13TestLetters = quickCheckResult (\cs -> (cs /= "" && lettersOnly cs) --> (cs  == rot13 (rot13 cs)) && ((rot13 cs) /= cs))
rot13TestNumbers = quickCheckResult (\cs -> (cs /= "" && numbersOnly cs) --> cs  == (rot13 cs))


lettersOnly :: [Char] -> Bool
lettersOnly [] = True
lettersOnly (c:cs) = c `elem` ['a'..'z']++['A'..'Z'] && lettersOnly cs

numbersOnly :: [Char] -> Bool
numbersOnly [] = True
numbersOnly (c:cs) = c `elem` ['0'..'9'] && numbersOnly cs

