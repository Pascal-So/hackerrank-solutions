import Control.Monad

type Point = (Int, Int)

next_move :: Point -> [String] -> String
next_move p field =
  let
    nr_dirt = sum $ map (length.filter (=='d')) field


main :: IO()
main = do
    pos_t <- getLine
    let pos = (\[a,b] -> (read a, read b) ) $ words pos_t :: (Int, Int)
    board <- replicateM 5 getLine
    putStrLn $ next_move pos board
