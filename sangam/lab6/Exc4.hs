{--
    Assignment:     Lab 6: Assignment 4
    Name:           Sangam Gupta
    Time spent:     1 hour
--}
module Exc4 where
    
import Data.List
import Lecture6

-- test (x:xs) = do k <- primeTestF x
--                  if k then return x else test xs

test :: [Integer] -> Int -> IO Integer
test (x:xs) k = do n <- primeTestsF k x
                   if n then return x else test xs k

main = do c <- test composites 1
          c1 <- test composites 2
          c2 <- test composites 3
          print c          
          print c1          
          print c2
          