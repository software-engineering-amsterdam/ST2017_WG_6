module Lab4
where 
import Data.List

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Source: https://hackage.haskell.org/package/Unique-0.4.7.1/docs/src/Data-List-Unique.html#sortUniq
sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort