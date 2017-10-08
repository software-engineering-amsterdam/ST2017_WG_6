module Ex3 where

import Data.List
import System.Random
import Lecture5

minSudGen :: IO Bool
minSudGen = do 
    [r] <- rsolveNs [emptyN]
    node@(sud, _) <- genProblem r
    return (uniqueSol node && all (not.uniqueSol.(eraseN node)) (filledPositions sud))

