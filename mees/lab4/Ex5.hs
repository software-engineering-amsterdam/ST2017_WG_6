module Ex5 where

import Data.List
import Data.Tuple
import Lab4
import Lecture4

symClos :: Ord a => Rel a -> Rel a
symClos x = sortUniq $ x ++ (map swap x)
