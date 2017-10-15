-- http://chrisdone.com/posts/measuring-duration-in-haskell
module Lab6_2
where
import Lab6_1
import Control.Exception
import Data.Time
import System.Random



{-
    Assignment:		Lab 6: Exercise 2
    Name:           Sara Oonk
    Time spent:     20.00 -
    Sources:        Using sara/lab6_1.hs
                    Measuring duration in Haskell: http://chrisdone.com/posts/measuring-duration-in-haskell

    Comments:       According to the used source, this is a wrong way to measure time. It is however a quick
                    and easy way to measure time without people having to install packages to run it, besides
                    there's no need for high accuracy here.


    ASSIGNMENT:
    Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.
-}

main = do
    print "_________________________________"
    b <- getStdRandom (randomR (0,50000))
    e <- getStdRandom (randomR (0,50000))
    m <- getStdRandom (randomR (0,50000))
    print ("exM "++show b++"^"++show e++"(mod "++show m++"): ")
    start <- getCurrentTime
    print (exM b e m)
    end <- getCurrentTime
    resultA <- return(diffUTCTime end start)
    print (resultA)
    print "- - - - - - - - - - - - - - - - -"

    print ("expM "++show b++"^"++show e++"(mod "++show m++"): ")
    start <- getCurrentTime
    print (exM b e m)
    end <- getCurrentTime
    resultB <- return(diffUTCTime end start)
    print (resultB)
    print "- - - - - - - - - - - - - - - - -"

