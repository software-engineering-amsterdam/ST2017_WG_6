{-
The lecture notes of this week discuss the notions of satisfiability, tautology, contradiction, logical entailment and logical equivalence for formulas of propositional logic.

The lecture notes give a definition of satisfiable, for objects of type Form.

Your task is to give definitions of:
<code>

Use a module that imports Lecture3.lhs or Lecture3.hs (commented out above). Check that your definitions are correct.

Deliverables: implementation, description of your method of checking the definitions, indication of time spent.

-}


module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction form =  not(satisfiable(form))



--
--
--tautology :: Form -> Bool
--
--
--
--
---- | logical entailment
--entails :: Form -> Form -> Bool
--
---- | logical equivalence
--equiv :: Form -> Form -> Bool
--

--testContradiction xs =

--
--testDefinition :: IO ()
--testDefinition def = testP 1 100 def
----
----"Build your own Quickcheck" variant
----testD :: Int -> Int -> IO ()
--testD k n check = if k == n then print (show n ++ " tests passed")
-- else do
--  xs <- randomForm 3
--  if (check xs) then
--   do
--    print ("pass on: " ++ show xs)
--    testD (k+1) n
--    else error ("failed test on: " ++ show xs)



--validChars :: [Char]
--validChars = [
-- ' ',
-- '(', ')', '*', '+', '-',
-- '0', '1', '2', '3', '4',
-- '5', '6', '7', '8', '9'
-- ]






---- REUSED SOURCES ---------------------------------
--
---- Sangam's randomForm (slight modification)
--randomForm :: Int -> IO Form
--randomForm 0 = do Prop <$> getRandomInt 4
--randomForm d = do q <- getRandomInt 4
--                  k <- randomForm (d-1)
--                  m <- randomForm (d-1)
--                  case q of
--                    0 -> do return $ Neg k
--                    1 -> do return $ Equiv (k) (m)
--                    2 -> do return $ Impl (k) (m)
--                    3 -> do return $ Cnj [(k), (m)] -- Limited to two
--                    4 -> do return $ Dsj [(k), (m)] -- Limited to two
--
--getRandomInt :: Int -> IO Int
--getRandomInt n = getStdRandom (randomR (0,n))
----





