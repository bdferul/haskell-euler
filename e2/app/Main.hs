module Main where

fib :: [Integer]
fib = f 1 2 where
    f a b = a : f b (b + a)

under_4mil :: Integer -> Bool
under_4mil x = x < 4_000_000

fib_under_4mil :: [Integer]
fib_under_4mil = takeWhile under_4mil fib

ans :: Integer
ans = sum [x | x <- fib_under_4mil, even x]

main :: IO ()
main = print ans
