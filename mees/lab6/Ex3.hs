import Lecture6
import Data.List
import Data.Char

composites' :: [Integer]
composites' = filter (not.prime) [2..]