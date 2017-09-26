{--
Assignment:		Lab 4: Assignment 5
Name:           Sangam Gupta
Time spent:     30 min
--}
module Exc5 where

import Data.Tuple
import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = sort $ union (fmap swap a) a

-- So the sematric closure of an set S = [(1,2), (2,3)] = S union inverse S