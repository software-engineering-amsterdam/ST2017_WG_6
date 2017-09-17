module Lab2 where

import Data.List
import Test.QuickCheck
import qualified Exc1RedCurry as Exc1
import qualified Exc2Triangles as Exc2
import qualified Exc3Properties as Exc3
import qualified Exc4Permutations as Exc4
import qualified Exc5Derangements as Exc5
import qualified Exc6ROT13 as Exc6
import qualified Exc7IBAN as Exc7

main = do
    putStrLn "\n***Lab2 output for 1. Red Curry***"
    Exc1.main

    putStrLn "\n***Lab2 output for 2. Recognizing triangles***"
    Exc2.main

    putStrLn "\n***Lab2 output for 3. Testing properties***"
    Exc3.main

    putStrLn "\n***Lab2 output for 4. Recognizing Permutations***"
    Exc4.main

    putStrLn "\n***Lab2 output for 5. Recognizing and generating derangements***"
    Exc5.main

    putStrLn "\n***Lab2 output for 6. Implementing and testing ROT13***"
    Exc6.main

    putStrLn "\n***Lab2 output for 7. Implementing and testing IBAN encoding***"
    Exc7.main
