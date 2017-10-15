-- http://chrisdone.com/posts/measuring-duration-in-haskell
module Lab6_2
where
import Lab6_1
import Control.Exception
import System.Random
import System.TimeIt

{-
    Assignment:		Lab 6: Exercise 2
    Name:           Sara Oonk
    Time spent:     1h
    Sources:        Using sara/lab6_1.hs
                    Measuring duration in Haskell: http://chrisdone.com/posts/measuring-duration-in-haskell
                    https://stackoverflow.com/questions/6766450/haskell-function-execution-time
                    http://0xax.blogspot.nl/2013/08/get-function-execution-time-in-haskell.html

    Comments:       Figured out during exercise 3 that my first time measuring method was too inaccurate, see below.
                    Turns out my implementation is still slower, too bad. :) Some output:

                    *Lab6_2> main
                    expM: 0
                    CPU time:   0.17s
                    exM: 0
                    CPU time:   2.59s


    ASSIGNMENT:
    Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.
-}


main = do
 timeIt $ putStrLn ("expM: " ++ show (expM 123 3456789 123))
 timeIt $ putStrLn ("exM: " ++ show (exM 123 3456789 123))















{-
BAD APPROACH!
It turned out that this method is too inaccurate this way; the second run is always relatively faster than the first so
swapping the order makes the other seem faster suddenly.


Original comments:
--
According to the used source, this is a wrong way to measure time. It is however a quick
and easy way to measure time without people having to install packages to run it, besides
there's no need for high accuracy here.
I chose to print instead of just evaluate, to make it take a little longer than "0.00001" secs
It turns out my implementation is slower than expM.
--


main = do
    print "_________________________________"
    b <- getStdRandom (randomR (0,50000))
    e <- getStdRandom (randomR (0,50000))
    m <- getStdRandom (randomR (0,50000))
    print (show b++"^"++show e++"(mod "++show m++") = ")

    startA <- getCurrentTime
    print (exM b e m)
    endA <- getCurrentTime
    resultA <- return(diffUTCTime endA startA)
    print ("exM:   "++show resultA)
    print "- - - - - - - - - - - - - - - - -"

    startB <- getCurrentTime
    print (expM b e m)
    endB <- getCurrentTime
    resultB <- return (diffUTCTime endB startB)
    print ("expM:  "++show resultB)
    print "- - - - - - - - - - - - - - - - -"




Some output:

*Lab6_2> main
"_________________________________"
"41218^44044(mod 35757) = "
19663
"exM:   0.152685s"
"- - - - - - - - - - - - - - - - -"
19663
"expM:  0.003649s"
"- - - - - - - - - - - - - - - - -"
*Lab6_2> main
"_________________________________"
"39764^11888(mod 9062) = "
6538
"exM:   0.011366s"
"- - - - - - - - - - - - - - - - -"
6538
"expM:  0.000608s"
"- - - - - - - - - - - - - - - - -"
*Lab6_2> main
"_________________________________"
"12794^33639(mod 45286) = "
24686
"exM:   0.001898s"
"- - - - - - - - - - - - - - - - -"
24686
"expM:  0.001664s"
"- - - - - - - - - - - - - - - - -"

-}
