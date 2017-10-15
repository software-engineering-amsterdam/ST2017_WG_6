
-- I was running bits of this directly in Exercises.hs, the combined file.
-- Though the implementations are the same, exM and exM' can get weird with high numbers, maybe due to memory.


-- using TimeIt


--
--
---- Exc3 Implemented, explained in Exercises.hs
--exM :: Integer -> Integer -> Integer -> Integer
--exM _ 0 _ = 1
--exM x y n = let z | testBit y 0 = mod x n
--                   | otherwise = 1
--             in mod (z * (exM (mod (x^2) n) (shiftR y 1) n)) n
--
---- Ex2 - Test exM' vs exM vs expM
---- See test data bottom of file. Results:
--
--
--
--
--
--




-- Test one at a time (swap and reload) for accurate, cacheless results
compareModExponentiation = do
-- timeIt $ putStrLn ("exM: " ++ show (exM 444 22222222 312))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 444 22222222 312))
-- timeIt $ putStrLn ("expM: " ++ show (expM 444 22222222 312))
-- putStrLn "---------------------------------"

-- timeIt $ putStrLn ("exM: " ++ show (exM 4444 222222222 312))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 4444 222222222 312))
-- timeIt $ putStrLn ("expM: " ++ show (expM 4444 222222222 312)) -- Can't handle at this point
-- putStrLn "---------------------------------"
--
-- timeIt $ putStrLn ("exM: " ++ show (exM 44444 2211222222 312))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 44444 2211222222 312))
---- timeIt $ putStrLn ("expM: " ++ show (expM 44444 2211222222 312)) -- Can't handle at this point
-- putStrLn "---------------------------------"

-- Trying to find a point where it becomes more than 0.00s ...

-- timeIt $ putStrLn ("exM: " ++ show (exM 9999 9191919191919191 4567))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 9999 9191919191919191 4567))
-- putStrLn "---------------------------------"

-- timeIt $ putStrLn ("exM: " ++ show (exM 9191919191919191 9191919191919191 4567))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 9191919191919191 9191919191919191 4567))
-- putStrLn "---------------------------------"
--
-- timeIt $ putStrLn ("exM: " ++ show (exM 91919191919191919191919191919191 91991919191919191911919191919191 45691919191919191917))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 91919191919919191919191919119191 91991919191919191911919191919191 45691919191919191917))
-- putStrLn "---------------------------------"

-- timeIt $ putStrLn ("exM: " ++ show (exM 999999999999999999999999999999999999999999999999999999 999999999999999999999999999999999999999999999999999999 45691919191919191917))
-- timeIt $ putStrLn ("exM': " ++ show (exM' 999999999999999999999999999999999999999999999999999999 999999999999999999999999999999999999999999999999999999 45691919191919191917))
-- putStrLn "---------------------------------"

-- Still 0.00s!