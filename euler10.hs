import Data.Array

primesBelow limit=
    let arr=(array (2::Int, limit-1) [(i,True)|i<-[2..limit-1]]) // [(a,False)|i<-[2..floor $ sqrt $ fromIntegral $ limit-1], a<-[2*i,3*i..limit-1]]    
    in [a|a<-[2..limit-1], arr!a]

main = print $ sum $ primesBelow 2000000
