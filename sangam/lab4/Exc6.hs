{--
Assignment:		Lab 4: Assignment 6
Name:           Sangam Gupta
Time spent:     30 min
--}

{-------------------------------------------------------------------------------------------------------------------------------------
6)  Use the datatype for relations from the previous exercise, plus

    to define a function

    trClos :: Ord a => Rel a -> Rel a 
    that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., 
    trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

    (Deliverable: Haskell program, indication of time spent.)
--------------------------------------------------------------------------------------------------------------------------------------}

-- infixr 5 @@

-- (@@) :: Eq a => Rel a -> Rel a -> Rel a
-- r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a 
-- trClos