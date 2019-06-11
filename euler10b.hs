import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

primesBelow limit=
    let a=runSTUArray $ do
        arr<-newArray (2, limit-1) True
        forM_ [2..limit-1] (\i-> do
            p <- readArray arr i
            when p $
                forM_ [i*2,i*3..limit-1] (\j->
                    writeArray arr j False))
        return arr
    in [p|(p,True)<-assocs a]

main = print $ sum $ primesBelow 2000000
