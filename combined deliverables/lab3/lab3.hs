module Lab3 where

import Data.List
import Test.QuickCheck
import qualified Exc1
import qualified Exc1ExtraTests
import qualified Exc2
import qualified Exc3
import qualified Exc4
import qualified Exc5

main = do
    putStrLn "\n*** Lab3 output for 1. Propositional logic notions ***"
    putStrLn "\n* Run manual tests *"
    Exc1.main
    putStrLn "\n* Run automated test *"
    Exc1ExtraTests.main

    putStrLn "\n*** Lab3 output for 2. Testing parse function ***"
    Exc2.main

    putStrLn "\n*** Lab3 output for 3. CNF converter ***"
    putStrLn "* Skipping, as converter is tested in exercise 4 *"

    putStrLn "\n*** Lab3 output for 4. Formula generator ***"
    Exc4.main

    putStrLn "\n*** Lab3 output for 5 (Bonus). CNF to clause converter ***"
    Exc5.main