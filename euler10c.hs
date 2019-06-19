isPrime :: Int->Bool
isPrime x = loop 2 where
    loop i =
        if i*i>x
            then True
            else if rem x i == 0 then False else loop (i+1)

primes = filter isPrime [2..]

main = print $ sum $ takeWhile (<2000000) primes