{-
When the Sudoku code was presented to an audience of Master of Logic students, a proposal emerged to refactor the code to make the formulation of constraints more uniform. The following definitions were proposed:

> type Position = (Row,Column)
> type Constrnt = [[Position]]
The regular constraints for Sudoku can now be stated as:

> rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
> columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
> blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
The generation of the values that are still possible at a given position now takes the following shape:

> freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
> freeAtPos' s (r,c) xs = let 
>    ys = filter (elem (r,c)) xs 
>  in 
>    foldl1 intersect (map ((values \\) . map s) ys)
Refactor the code along the lines of this proposal, and next compare the two versions for extendability and efficiency. Which of the two versions is easier to modify for NRC sudokus, and why? Which of the two versions is more efficient? Devise your own testing method for this, and write a short test report.

Deliverables: Refactored code, test report, indication of time spent.
-}

-- 1 uur