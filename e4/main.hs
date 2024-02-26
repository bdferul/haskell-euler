(<|) f = f

(|>) a f = f a

palindrome w = w == reverse w

descendFrom x = [x, x - 1 ..]

largestThreeDigitPalindrome x = f False start start
  where
    start = descendFrom 999
    f turn (a : axs) (b : bxs) =
      let res = a * b
       in if palindrome res
            then res
            else
              if turn
                then f False axs (b : bxs)
                else f True (a : axs) bxs

main = descendFrom 10 |> take 15 |> print
