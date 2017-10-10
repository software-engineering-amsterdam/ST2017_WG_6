import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)
import Data.Bits
import System.Random

genKeys :: Integer -> Integer -> IO ((Integer, Integer), (Integer, Integer))
genKeys x y = do
  let n = x * y
  let phi = (x - 1) * (y - 1)
  e <- getStdRandom (randomR (1, phi))
  when (coprime e phi) (print (show phi ++ " and " ++ show e ++ " are coprime!"))
  let d = invM e phi
  return ((e, n),(d, n))

encrypt :: (Integer, Integer) -> [Char] -> [Integer]
encrypt pk txt = map (\x -> ((fromIntegral (ord x)) ^ (fst pk)) `mod` (snd pk)) txt

decrypt :: (Integer, Integer) -> [Integer] -> [Char]
decrypt pk txt = map (\x -> (chr (fromIntegral ((x ^ (fst pk)) `mod` (snd pk))))) txt


man msg = do
  (pubKey, privKey) <- genKeys 17 23
  let secretMsg = encrypt privKey msg
  print ("Public key:" ++ show pubKey ++ " Private key: " ++ show privKey)
  print (secretMsg)
  print (decrypt pubKey secretMsg)

--  VERSON 2.0
-- import Lecture6
-- import Data.List
-- import Data.Char
-- import Control.Monad (when)
-- import Data.Bits
-- import System.Random
-- import Data.Maybe (listToMaybe)
-- import Numeric    (readInt)

-- strToInt x = read (intStr x) :: Integer
--     where 
--         intStr [] = []
--         intStr (xh:xt) = (to3Digits (show (ord (xh)))) ++ intStr xt

--         to3Digits x | ((length x) - 3) == 0 = x
--                     | otherwise = to3Digits ("0" ++ x)

-- intToStr x = strInt (splitAt 3 (reverse (show x)))
--     where
--         strInt ("",_) = []
--         strInt (x ,y) = strInt (splitAt 3 y) ++ [chr(fromIntegral(read (reverse x) :: Integer))]

-- man msg x y = do
--   let (pubKey, privKey) = (rsaPublic x y, rsaPrivate x y)
--   let secretMsg = rsaEncode privKey (strToInt msg)
--   print ("Public key:" ++ show pubKey ++ " Private key: " ++ show privKey)
--   print (secretMsg)
--   print (intToStr (rsaDecode pubKey secretMsg))

