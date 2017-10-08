module Lab5_3 where
    
import Data.List
import System.Random
import Lecture5

test = do   [r] <- rsolveNs [emptyN]
            s <- genProblem r
            showNode s
            print "Original has a unique solution?"
            print (uniqueSol s)
            print "Is the solution trully unique?"
            print (all (checkUniqueSolution s) (filledPositions (fst s)))

checkUniqueSolution :: Node -> (Row,Column) -> Bool
checkUniqueSolution n (r,c)= not (uniqueSol (eraseN n (r,c)))