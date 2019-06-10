import Data.List

answer1 = maximum [p |x <- [999,998..100], y <- [999,998..x], let p = x * y, let s=show p, s==reverse s]

maxFilter filter1 lst temp =
    case lst of
        [] -> temp
        (x:xs) ->
            if x > temp
                then if filter1 x
                    then maxFilter filter1 xs x
                    else maxFilter filter1 xs temp
                else maxFilter filter1 xs temp

answer2 = maxFilter (\x->let s = show x in s==reverse s) [x*y|x <- [999,998..100], y <- [999,998..x]] 0


answer3 = foldl' (\x y->if y > x && palindrome y then y else x) 0 [x*y|x <- [999,998..100], y <- [999,998..x]]
    where palindrome p = let s=show p in s==reverse s

main = do
    print answer1
    print answer2
    print answer3