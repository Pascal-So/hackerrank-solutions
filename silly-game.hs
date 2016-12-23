import Control.Monad

primes_up_to :: Int -> [Int]
primes_up_to n =
  let
    has_divisor_in a lst =
      not $ null $ filter (\x -> a `mod` x == 0) lst
  in
    reverse $ foldl (\a x -> if x `has_divisor_in` a then a else x:a) [] [2..n]

who_wins :: [Int] -> Int -> String
who_wins primes n =
  let nr_primes = length $ takeWhile (<=n) primes
  in
    if nr_primes `mod` 2 == 0 then
      "Bob"
    else
      "Alice"

solve :: [Int] -> [String]
solve arr =
  let
    max_n = maximum arr
    primes = primes_up_to max_n
  in
    map (who_wins primes) arr

main = do
  g <- readLn
  a <- replicateM g readLn
  putStrLn $ unlines $ solve a
