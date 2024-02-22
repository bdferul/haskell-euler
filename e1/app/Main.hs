module Main where

main :: IO ()
main = print ans where 
    ans = sum lst
    lst = [x | x <- [3..999], valid x]
    valid x = x `mod` 3 == 0 || x `mod` 5 == 0
