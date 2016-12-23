import Control.Monad
import Debug.Trace

modNr :: Int
modNr = 10^8 + 7

modsum :: Int -> [Int] -> Int
modsum _ [] = 0
modsum m (x:xs) =
  (x + (modsum m xs)) `mod` m

calculateNext :: [Int] -> [Int]
calculateNext lst =
  let
    z = zip lst $ reverse lst
    n = modsum modNr $ map (uncurry (*)) z
  in
    lst ++ [n]

treePoss :: Int -> [Int]
treePoss n =
  (iterate calculateNext [1])!!n

solve :: [Int] -> [Int]
solve lst =
  let
    n = maximum lst
    poss = treePoss n
  in
    map (poss!!) lst

main = do
  n <- readLn
  lst <- replicateM n readLn
  putStrLn $ unlines . map show $ solve lst
