-- ################################################
-- # Mees Kalf
-- # Excersize 2
-- # 2 hour(s)
-- ################################################
module Ex7 where

import Data.List
import System.Random
import Lecture5
import Ex1

-- Get all values of the sudoku which are not 0
sudValues :: Lecture5.Sudoku -> [Lecture5.Value]
sudValues s = filter (/=0) [ s (r',c') | r' <- Ex1.positions , c' <- Ex1.positions ]


-- Return the amount of filled values in the sudoku
lenFilled fSolve fGen = do [r] <- fSolve [Ex1.emptyN]
                           s <- fGen r
                           return (length (sudValues (fst s)))


-- Calculate the average length of n generated minimal sudokus
avrLen :: (Fractional b, Monad m) => ([Ex1.Node] -> m [t])-> (t -> m (Ex1.Sudoku, b1)) -> Int -> m Double
avrLen fs fg n = do
        te <- sequence (replicate n (lenFilled fs fg))
        let m = (fromIntegral (sum te)) / (fromIntegral n)
        return m

-- Calculate the average length for normal minimal generated sudokus
avrFilledSud n = avrLen Lecture5.rsolveNs Lecture5.genProblem n

-- Calculate the average length for NRC minimal generated sudokus
avrFilledNrc n = avrLen Ex1.rsolveNs Ex1.genProblem n

{-----------------------------------------------------------------------------

The generation of tests is quite slow, so we on purpose didn't provide a 
quickCheck function, this will simply take to long.

We created 2 function, avrFilledSud and avrFilledNrc, both calulcating the
average of the amount of values in a minimalized generated sudoku.

Results:
avrFilledSud gave an average of 23.9 over 40 generated sudokus
avrFilledNrc gave an average of 16.8 over 40 generated sudokus

So the minimal form of Nrc needs way less hints to be solved 

-----------------------------------------------------------------------------}