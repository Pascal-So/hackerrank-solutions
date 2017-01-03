import Control.Monad

modNr :: Int
modNr = 10^8 + 7

getFibs :: Int -> [Int]
getFibs n =
  map fst $ scanl (\(a,b) _ -> (b, (a+b) `mod` modNr)) (0,1) [1..n]

solve :: [Int] -> [Int]
solve nums =
  let
    mx = maximum nums
    fibs = getFibs mx
  in
    map (fibs!!) nums

main = do
  n <- readLn
  nums <- replicateM n readLn
  putStrLn $ unlines. map show $ solve nums
