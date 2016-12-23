import Control.Monad

solve :: Int -> Int
solve n =
  (n+1)*3*n/2 - n*2

main = do
  n <- readLn
  lst <- replicateM n readLn
  putStrLn $ unlines . map (show.solve) $ lst
