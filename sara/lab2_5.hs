module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


{-
    Implementing and testing ROT13 encoding

    ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers. See also www.rot13.com.

    First, give a specification of ROT13.

    Next, give a simple implementation of ROT13.

    Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.

    10:30
-}