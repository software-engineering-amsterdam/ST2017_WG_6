module Ex4 where

import Data.List
import System.Random
import Lecture5

blockPos :: [[(Row, Column)]]
blockPos = concat $ map (\x -> map (\y -> [(i,j)|i <- y ,j <- x]) blocks) blocks

diffSeq :: Int -> [[(Row, Column)]] -> [[[(Row, Column)]]]
diffSeq n bp = filter ((== n).length) (subsequences bp)


--  TODO second let can be [] instead of [(sol, _)] that's why man4 crashes sometimes
man n | n > 4 = print "More than 4 empty blocks is mathematically impossible"
      | otherwise = do 
        [r] <- rsolveNs [emptyN]
        bp <- randomize blockPos
        let tr = map ((foldl eraseN r).concat) (diffSeq n bp)
        let [(sol, _)] = take 1 (filter (snd) (zip tr (map uniqueSol tr)))
        showNode sol

