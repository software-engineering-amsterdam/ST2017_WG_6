{----------------------------------------------------------------------------------------------------------------------
    NAME:       Sara Oonk
    ASSIGNMENT: Lab 3, exercise 2 - Testing 'parse'
    TIME TAKEN:
         TOTAL: 12h (Interesting and informative but recommending myself not to take it this far next time)


    SUMMARY OF REPORT: After careful preparation and design I ended up having to give up on the chosen approach, even after
    consulting Hugo (.hs file "tuesdayafternoon..." in Sara's personal directory is the state we left it in after a final attempt).
    Below text describes the theory and plans I had, the given code is finally a simpler implementation using Sangam's generator
    for exercise 4. At the bottom some disabled code is left containing the final attempt before deciding to finish it simply.

    Though it was an interesting process and a fun challenge I will refrain from spending 12h on an exercise like this in the future.

    REPORT:
                3h preparation:
                        - Studying lecture 3 (mainly to fully understand 'parse' and related functions)
                        - Studying lecture 2 for test methods and better grasping of test properties
                        - Considering approaches, proposed them to Hugo & Ana
                        - Finalizing approach, structuring and documenting
                1h defining properties, roughly designing my test by studying parser and lexer further
                5h trying to implement, concluded design flaw. Talked to Hugo, tried to get it working together.
                The state we left it in is the file: tuesdayafternoon_lab3_2.hs

                Came up with two options with Hugo:
                    -   Simplify the generator by discarding "<=>" and "==>" so that it can work with single Chars instead of String lists.
                        This way you can't check for proper "<=>" and "==>" tokens but this way the generator actually generates
                        something that meets the type of 'parse' and therefore solves conflicts with pre- and postconditions and the
                        need to concatenate stuff in the middle of the Hoare test (after checking the precondition) which is
                        very fishy to begin with.
                    -   Implement the use of a generator that was made as solution for exercise 4

                2h - Just keep getting stuck on IO types, can't seem to close the gap anywhere. Didn't really know much about IO beforehand.
                1h - Implemented an automated test using Sangam's formula generator, which only generates syntactically correct formulas.
                My precondition check is limited to exclude "<" ">" and "=" characters for simplicity due to time spent.
                ____

    SOURCES:    - Lecture 2 for Hoare testing and random Ints
                - Lecture 3

    DESCRIPTION:
    The lecture notes of this week define a function parse for parsing propositional formulas.
    Test this function. You can use any test method you want.
    Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

    APPROACH:
    ---- AUTOMATED VS MANUAL ----
    Automated test methods are most valuable when they draw their domain (test data) from a generator, because this way
    the domain is the largest and most random, not decided by human hands, and not limited to ideas that may not be complete.

    ---- TESTING USING GENERATORS ----
    The Hoare Test with relevance is an interesting way to perform automated tests whilst making sure that the randomly
    generated data set is actually relevant* (enough) in order to be able to call the test reliable. However, for testing
    'parse' I would require a generator that generates formulas, which is a little more complex than just random int lists.
    *(test input is called relevant when it meets the precondition of the test.)

    ---- REQUIREMENTS OF MY GENERATOR ----
    If I want to do Hoare testing with relevance, it would not make sense if all of my generated formulas match the
    precondition of the test, because then the relevance would always be 1.0. So if I want to test 'parse' in an automated
    way while simultaneously exploring the concept of relevance, my formula generator must return imperfect formulas as
    well as proper formulas. This means I will let my generator throw in some characters that are not recognized as tokens
    by the lexer. After drawing conclusions, I will remove the invalid characters from the domain of the generator to
    verify 100% relevance.

    My approach for the generator turned out to cause complicated conflicts between the pre- and postcondition and 'parse' itself.
    I simplified the generator by removing "==>" and "<=>" so that I could at least make my implementation work.
    Sadly, though this helped a little, I was still unable to close the gap between IO and non-IO types in this construction.

    I will provide a more standard solution by using a generator that was made for exercise 4, and disregard testing with
    relevance for the moment.

    ---- TEST PROPERTIES ----
    - 'parse' only accepts Strings and throws an error when unknown tokens are passed. So;
    PRECONDITION: Any String consisting only of tokens recognized by the lexer: ['(', ')', '*', '+', '-', "==>" "<=>"]

    - Leftover tokens are not shown in the result of the function 'parse'.
    - If input is unparsable, output must be empty.
      - Input is unparsable when order of tokens conflicts with grammar rules of lexer (to conclude from its implementation).
      - Input is unparsable when number of opening parentheses does not equal the number of closing parentheses.

    POSTCONDITION: If parsable: - The String representation of the result must be equal to the start of the input (ignoring spaces).
                                - Characters after the last closing parenthesis must not be shown in the result.
                   Otherwise: The result must be the empty list.

SEE SUMMARY ABOVE
-----------------------------------------------------------------------------------------------------------------------}


module Exc2 where
import Lecture3
import System.Random
import System.IO
import Exc4

-- Main function --
mainn :: IO ()
mainn = testParse 1 100


-- PROPERTIES
precondition :: String -> Bool
precondition [] = True
precondition (c:cs) = elem c validChars && precondition cs

postcondition :: String -> Bool
postcondition form = form == (show(head(parse form)))


--"Build your own Quickcheck" variant
testParse :: Int -> Int -> IO ()
testParse k n = if k == n then print (show n ++ " tests passed")
 else do
  xs <- randomForm 3
  if ((precondition (show xs)) --> (postcondition (show xs))) then
   do testParse (k + 1) n
  else error ("failed test on: " ++ show xs)


--"<=>" "==>" removed for simplicity after overcomplicated experiment, see notes above and/or .hs file "tuesdayafternoon..."
validChars :: [Char]
validChars = [
 ' ',
 '(', ')', '*', '+', '-',
 '0', '1', '2', '3', '4',
 '5', '6', '7', '8', '9'
 ]







-- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! --
--------------- FAILED EXPERIMENT ---------------------------------------------------------------------------------------
-- DOMAIN
-- Removed "==>" and "<=>" to at least make my implementation work after a long experiment
--validTokens :: [Char]
--validTokens = [
-- ' ',
-- '(', ')', '*', '+', '-',
-- '0', '1', '2', '3', '4',
-- '5', '6', '7', '8', '9'
-- ]
--
---- These are added to the generator's domain to measure Hoare relevance
--invalidTokens :: [Char]
--invalidTokens = ['p', 'q', 'r', 'x', 'y']
--
--genDomain :: [Char]
--genDomain = validTokens++invalidTokens
--
--
--
--parseS s = do show(parse s)
--
--testParseWithRelevance = do
-- fs <- return (genFormStrings 100 [])
-- hoareTestRc precondition parseS postcondition fs

-- GENERATOR
--genFormStrings :: Int -> [String] -> [String]
--genFormStrings 0 fs = fs
--genFormStrings n fs = genFormStrings (n-1) (fs ++ (return (concat genRandomListTokens)))

--
--genRandomListTokens :: [String]
--genRandomListTokens = do
-- ns <- return genIntList'
-- return (mapIntsToTokens ns [])


--mapIntsToTokens :: IO [Int] -> [Char] -> String
--mapIntsToTokens [] cs = cs
--mapIntsToTokens ns cs = do mapIntsToTokens (tail ns) (([genDomain !! (head ns)])++cs)


-- Modified from Lecture 2 -----------------------------------
--genIntList' :: IO [Int]
--genIntList' = do
--  k <- getRandomInt (length(validTokens++invalidTokens) -1)
--  n <- getRandomInt 30
--  getIntL' k n
--
--getIntL' :: Int -> Int -> IO [Int]
--getIntL' _ 0 = return []
--getIntL' k n = do
--   x <-  getRandomInt k
--   xs <- getIntL' k (n-1)
--   return (x:xs)
--
--
--
--getRandomInt :: Int -> IO Int
--getRandomInt n = getStdRandom (randomR (0,n))
--
--hoareTestRc ::  Fractional t =>
--               (a -> Bool)
--               -> (a -> a) -> (a -> Bool) -> [a] -> (Bool,t)
--hoareTestRc precond f postcond testcases = let
--       a = fromIntegral (length $ filter precond testcases)
--       b = fromIntegral (length testcases)
--     in
--       (all (\x ->
--         precond x --> postcond (f x)) testcases,a/b)
--
--------------- FAILED EXPERIMENT ---------------------------------------------------------------------------------------
-- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! -- !!! --


