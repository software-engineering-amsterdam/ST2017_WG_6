module Lab3 where

import Data.List
import Test.QuickCheck
import qualified Exc1
import qualified Exc2
import qualified Exc3
import qualified Exc4
import qualified Exc5

main = do
    putStrLn "\n*** Lab3 output for 1. Propositional logic notions ***"
    Exc1.main

    putStrLn "\n*** Lab3 output for 2. Testing parse function ***"
    Exc2.main

    putStrLn "\n*** Lab3 output for 3. CNF converter ***"
    Exc3.main

    putStrLn "\n*** Lab3 output for 4. Formula generator ***"
    Exc4.main

    putStrLn "\n*** Lab3 output for 5 (Bonus). CNF to clause converter ***"
    Exc5.main