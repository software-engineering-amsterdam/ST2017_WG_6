import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)
import Data.Bits
import Data.Text (chunksOf)
import System.Random
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

strToInt x = read (intStr x) :: Integer
    where 
        intStr [] = []
        intStr (xh:xt) = (to3Digits (show (ord (xh)))) ++ intStr xt

        to3Digits x | ((length x) - 3) == 0 = x
                    | otherwise = to3Digits ("0" ++ x)

intToStr x = strInt (splitAt 3 (reverse (show x)))
    where
        strInt ("",_) = []
        strInt (x ,y) = strInt (splitAt 3 y) ++ [chr(read (reverse x) :: Int)]

man msg x y = do
  let (pubKey, privKey) = (rsaPublic x y, rsaPrivate x y)
  let encMsg = rsaEncode privKey (strToInt msg)
  print (encMsg)
  print (intToStr (rsaDecode pubKey encMsg))
