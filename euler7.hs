isPrime :: Int->Bool
isPrime x = loop 2 where
    loop i =
        if i*i>x
            then True
            else if mod x i == 0 then False else loop (i+1)

main = print $ (filter isPrime [2..]) !! 10000