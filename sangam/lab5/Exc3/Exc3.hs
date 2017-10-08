{--
Assignment:     Lab 5: Assignment 3
Name:           Sangam Gupta
Time spent:     1 hour
--}
module Lab5 where
    
import Data.List
import System.Random
import Lecture5

{-
    We check whether the generated problem is unique by removing an element (r,c) from the node. 
    After removing we check if the solution is still unique. We do this for all elements.
    If we can find no element for which after removing it the solution is still unique then 
    we can conlude that the current generated problem is indeed minimal.
-}
test = do   [r] <- rsolveNs [emptyN]
            s <- genProblem r
            showNode s
            print "Original has a unique solution?"
            print (uniqueSol s)
            print "Is the solution trully unique?"
            print (all (checkUniqueSolution s) (filledPositions (fst s)))

checkUniqueSolution :: Node -> (Row,Column) -> Bool
checkUniqueSolution n (r,c)= not (uniqueSol (eraseN n (r,c)))