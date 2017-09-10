-- This module automatically tests the four individual files and outputs the results/performance. To run, load this module in GHCI and call 'main' method

-- TODO:
-- - Replace GHCI module import system with Cabal project
-- - Output test times for quickcheck methods
-- -
-- - 

module LabTest where
import Data.List
import Test.QuickCheck
import qualified Lab1Tim as Tim1
import qualified Lab2Tim as Tim2

main = do
    putStrLn "\nLab1 output for Tim:" 
    putStrLn "\nEXERCISE 4:" 
    putStrLn "Tim:" 
    print Tim1.answer4
    -- putStrLn "Mees:" 
    -- print Mees1.answer4
    -- putStrLn "Sara:" 
    -- print Sara1.answer4
    -- putStrLn "Sangam:"
    -- print Sangam1.answer4 


    putStrLn "\nEXERCISE 5:" 
    putStrLn "Tim:" 
    print Tim1.answer5
    -- putStrLn "Mees:" 
    -- print Mees1.answer5
    -- putStrLn "Sara:" 
    -- print Sara1.answer5
    -- putStrLn "Sangam:"
    -- print Sangam1.answer5 

