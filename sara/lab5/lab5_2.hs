module Lab5_2

where

import Data.List
import System.Random

{-
    Assignment:		Lab 5: Exercise 2
    Name:           Sara Oonk
    Time spent:
    Sources:        Lecture 5
    Comments:

Exercise 2

When the Sudoku code was presented to an audience of Master of Logic students, a proposal emerged to refactor the code
to make the formulation of constraints more uniform. The following definitions were proposed:

> type Position = (Row,Column)
> type Constraint = [[Position]]
The regular constraints for Sudoku can now be stated as:

> rowConstraint = [[(r,c)| c <- values ] | r <- values ]
> columnConstraint = [[(r,c)| r <- values ] | c <- values ]
> blockConstraint = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

The generation of the values that are still possible at a given position now takes the following shape:

> freeAtPos' :: Sudoku -> Position -> Constraint -> [Value]
> freeAtPos' s (r,c) xs = let
>    ys = filter (elem (r,c)) xs
>  in
>    foldl1 intersect (map ((values \\) . map s) ys)


Refactor the code along the lines of this proposal, and next compare the two versions for extendability and efficiency.
Which of the two versions is easier to modify for NRC sudokus, and why?
Which of the two versions is more efficient? Devise your own testing method for this, and write a short test report.

Deliverables: Refactored code, test report, indication of time spent.

--}

type Row        = Int
type Column     = Int
type Value      = Int
type Grid       = [[Value]]
type Node       = (Sudoku,[Position])
type Sudoku     = Position -> Value
type Position   = (Row,Column)
type Constraint = [[Position]] --Empty possible spots

-- OLD type Constraint = (Row,Column,[Value]) are now more like Positions

coordinates, values :: [Int]
coordinates = [1..9]
values    = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")


sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> Position -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

--TODO ADD TYPE
rowConstraint    = [[(r,c)| c <- values ] | r <- values ]
columnConstraint = [[(r,c)| r <- values ] | c <- values ]
blockConstraint  = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
allConstraints   = rowConstraint ++ columnConstraint ++ blockConstraint

-- All possible values for this pos
freeAtPos :: Sudoku -> Position -> Constraint -> [Value]
freeAtPos s (r,c) xs = let
   ys = filter (elem (r,c)) xs
 in
   foldl1 intersect (map ((values \\) . map s) ys)


-- Returns the range of indexes (together a block) that this single index belongs to
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks

-- Returns the values that are inside the subgrid of given location
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c ]

-- Returns the values 1-9 that aren't taken yet in the given list of values
freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq

-- Returns the values that aren't taken yet in an entire row of a Sudoku
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =
  freeInSeq [ s (r,i) | i <- coordinates  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =
  freeInSeq [ s (i,c) | i <- coordinates ]

freeInSubgrid :: Sudoku -> Position -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))




injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where
   vs = filter (/= 0) [ s (r,i) | i <- coordinates ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where
   vs = filter (/= 0) [ s (i,c) | i <- coordinates ]

subgridInjective :: Sudoku -> Position -> Bool
subgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- coordinates ]
                ++
               [ colInjective s c |  c <- coordinates ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]

extend :: Sudoku -> (Position, Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x


showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = slvd
--null . snd

slvd :: Node -> Bool
slvd (s,ps) = length(filledPositions s) == 81 && consistent s

-- Tested, does generate nodes.
extendNode :: Node -> Position -> [Node]
extendNode (s,positions) pos =
   [(extend s (pos, v), positions) | v <- (freeAtPos s pos allConstraints) ]


initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
              if (not . consistent) s then []
              else [(s, (openPositions s))]

openPositions :: Sudoku -> [Position]
openPositions s = [ (r,c) | r <- coordinates,
                            c <- coordinates,
                            s (r,c) == 0 ]

--length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
--length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

--TODO not sure about this one
positions :: Sudoku -> [(Position, [Value])]
positions s = --sortBy length3rd
    [(pos, freeAtPos s pos allConstraints) |
                        pos <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8],
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

emptyN :: Node
emptyN = (\ _ -> 0,openPositions (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs
                  if null y
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Position -> Position -> Bool
sameLen xs ys = length xs == length ys

getRandomPos :: [Position] -> IO [Position]
getRandomPos cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

-- Tested, working. Takes a random position,
-- extends node (minus this random position) from this position
rsuccNode :: Node -> IO [Node]
rsuccNode (s,ps) = do xs <- getRandomPos ps
                      if null xs
                        then return []
                        else return
                          (extendNode (s,ps\\xs) (head xs))


rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node])
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes =
  do xs <- ionodes
     if null xs
       then return []
       else
         if goal (head xs)
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys
                      then return [head ys]
                      else if null (tail xs) then return []
                           else
                             rsearch
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode


-- Tested, had to change last pattern, then it generated a solution once ...
uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:zs) = False


-- Tested, both erase are working
eraseS :: Sudoku -> Position -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)
eraseN :: Node -> Position -> Node
eraseN n (r,c) = (s, openPositions s)
  where s = eraseS (fst n) (r,c)


minimalize :: Node -> [(Row, Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [Position]
filledPositions s = [ (r,c) | r <- coordinates,
                              c <- coordinates, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
--
--mainpls = do s <- genProblem debugNode
--             showNode s

-- DEBUG DATA ----------------------------------------------------------------------------------------------------------
debug :: Grid
debug =      [[9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9],
              [9,9,9,9,9,9,9,9,9]]


debugSolution = [[8,9,1,6,4,2,5,7,3],
                 [5,6,3,7,1,8,2,9,4],
                 [7,4,2,3,5,9,8,6,1],
                 [3,5,9,1,8,6,7,4,2],
                 [6,2,8,4,7,5,3,1,9],
                 [1,7,4,9,2,3,6,8,5],
                 [4,8,6,5,3,1,9,2,7],
                 [2,1,5,8,9,7,4,3,6],
                 [9,3,7,2,6,4,1,5,8]]

debugSudoku :: Sudoku
debugSudoku = grid2sud example1

solsud = grid2sud debugSolution


debugNode :: Node
debugNode = ((grid2sud example1), openPositions (grid2sud example1))


