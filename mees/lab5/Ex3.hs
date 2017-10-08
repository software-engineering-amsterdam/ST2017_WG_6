module Ex3 where

import Data.List
import System.Random
import Lecture5

minSudGen :: IO Bool
minSudGen = do 
    [r] <- rsolveNs [emptyN]
    node@(sud, _) <- genProblem r
    let allNotUnique = all (not.uniqueSol.(eraseN node)) (filledPositions sud)
    if uniqueSol node && allNotUnique
    then return True
    else return False

