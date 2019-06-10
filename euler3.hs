maxPrimeFactor n = factor n 1 2
  where
    factor n p i
      | n == 1 = p
      | (mod n i) == 0 = factor (div n i) i i
      | otherwise = factor n p (i+1)

main = do
  print $ maxPrimeFactor 600851475143