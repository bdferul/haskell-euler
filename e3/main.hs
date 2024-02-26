(|>) a f = f a

(<|) f = f

(//) = div

(%) = mod

lpfOf = 600_851_475_143 % 5

primes = filterPrime (2 : [3, 5 ..])
  where
    filterPrime (p : xs) = p : filterPrime [x | x <- xs, x % p /= 0]

primeFactors = f primes
  where
    f (p : xs) x
      | p * p >= x = x
      | x `mod` p == 0 = f (p : xs) (x // p)
      | otherwise = f xs x

main = primeFactors lpfOf |> print
