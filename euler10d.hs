import Data.Array
import Control.Monad

-- very slow
primesBelow limit=
    loop 2 (array (2,limit-1) [(i,True)|i<-[2..limit-1]]) where
        loop i a =
            if i*i<=limit-1 
                then if a ! i
                    then loop (i+1) (a // [(j,False)|i<-[2..limit-1], j<-[2*i,3*i..limit-1]])
                    else loop (i+1) a
                else [p|p<-[2..limit-1], a ! p]
main = print $ sum $ primesBelow 2000000
