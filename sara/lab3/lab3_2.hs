{----------------------------------------------------------------------------------------------------------------------
    NAME:       Sara Oonk
    ASSIGNMENT: Lab 3, exercise 2 - Testing 'parse'
    TIME TAKEN: 3h preparation:
                        - Studying lecture 3 (mainly to fully understand 'parse' and related functions)
                        - Studying lecture 2 for test methods and better grasping of test properties
                        - Considering approaches, proposed them to Hugo & Ana
                        - Finalizing approach, structuring and documenting
                1h defining properties, roughly designing my test by studying parser and lexer further
                ____
          TOTAL:

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

-----------------------------------------------------------------------------------------------------------------------}


module Lab3_2
where
import Lecture2
import Lecture3

-- DOMAIN
-- Each element is a String/[Char] so that "==>" and "<=>" can each be evaluated as one token
validTokens :: [[Char]]
validTokens = [
 " ", "==>", "<=>",
 "(", ")", "*", "+", "-",
 "0", "1", "2", "3", "4",
 "5", "6", "7", "8", "9"
 ]

-- These are added to the generator's domain to measure Hoare relevance
invalidTokens :: [[Char]]
invalidTokens = ["p", "q", "r", "x", "y"]

genDomain :: [[Char]]
genDomain = validTokens++invalidTokens



-- PROPERTIES
precondition :: [[Char]] -> Bool
precondition [] = True
precondition (c:cs) = elem c validTokens && precondition cs

--["1", "2," "==>"]

-- isParsable
  -- equalParenthesis
  -- correctGrammar

-- noRemainder
-- ( = +1
-- ) = -1
-- _ && > 0 ? False
--

testParse = do
 cs <- genRandomListTokens
 form <- return (concat cs)
 print form


-- GENERATOR
genRandomListTokens :: IO [[Char]]
genRandomListTokens = do
 ns <- genIntList'
 return (mapIntsToTokens ns [])


mapIntsToTokens :: [Int] -> [[Char]] -> [[Char]]
mapIntsToTokens [] cs = cs
mapIntsToTokens (n:ns) cs = mapIntsToTokens ns (([genDomain !! n])++cs)


-- Modified from Lecture 2 -----------------------------------
genIntList' :: IO [Int]
genIntList' = do
  k <- getRandomInt (length(validTokens++invalidTokens) -1)
  n <- getRandomInt 30
  getIntL' k n

getIntL' :: Int -> Int -> IO [Int]
getIntL' _ 0 = return []
getIntL' k n = do
   x <-  getRandomInt k
   xs <- getIntL' k (n-1)
   return (x:xs)
---------------------------------------------------------------
