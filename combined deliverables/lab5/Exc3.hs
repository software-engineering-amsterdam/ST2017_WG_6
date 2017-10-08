{--
Assignment:     Lab 5: Assignment 3
Name:           Sangam Gupta
Time spent:     1 hour
--}
module Exc3 where
    
import Data.List
import Lecture5

{-
    We check whether the generated problem is unique by removing an element (r,c) from the node. 
    After removing we check if the solution is still unique. We do this for all elements.
    If we can not find an unique solution after removing an element then 
    we can conlude that the current generated problem is indeed minimal.
-}
test = do [r] <- rsolveNs [emptyN]
          s <- genProblem r
          showNode s
          print "Original has a unique solution?"
          print (uniqueSol s)
          print "Is the solution trully unique?"
          print (all (not . uniqueSol . eraseN s) (filledPositions (fst s)))