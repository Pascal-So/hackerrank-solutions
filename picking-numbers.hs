import Control.Applicative
import Control.Monad
import System.IO
import Data.List

get_combination :: [(Int, Int)] -> [(Int, Int)]
get_combination arr =
  let num = fst $ head arr
  in
    takeWhile (\p -> fst p - num <= 1) arr

max_subset :: [Int] -> Int
max_subset arr =
  let
    groups = map (\a -> (head a, length a)) $ group $ sort arr
    valid_combinations = map get_combination $ init $ tails groups
  in
    maximum $ map (sum . map snd) valid_combinations
    

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    putStrLn $ show $ max_subset a
