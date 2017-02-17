import Control.Monad
import qualified Data.Vector as Vec

data Tree = Node Int Tree Tree | Leaf

instance Show Tree where
    show Leaf = ""
    show (Node n l r) = (show l) ++ (show n) ++ " " ++ (show r)


swapLevel :: Int -> Int -> Tree -> Tree
swapLevel _ _ Leaf = Leaf
swapLevel level k (Node i l r) =
    let
        left = swapLevel (level+1) k l
        right = swapLevel (level+1) k r
    in
        if level `mod` k == 0 then
            Node i right left
        else
            Node i left right

constructTree :: [(Int, Int)] -> Tree
constructTree children =
    let
        vec = Vec.fromList children :: Vec.Vector (Int, Int)
        construct (-1) = Leaf
        construct i =
            let
                (leftId,rightId) = vec Vec.! (i-1)
            in
                Node i (construct leftId) (construct rightId)
    in
        construct 1

toPair :: [a] -> (a, a)
toPair [a,b] = (a, b)

solve :: [(Int, Int)] -> [Int] -> [Tree]
solve childrenList queries =
    let
        t = constructTree childrenList
    in
        tail $ scanl (flip (swapLevel 1)) t queries

main = do
    n <- readLn
    childrenList <- fmap (map (toPair . map read . words)) $ replicateM n getLine
    q <- readLn
    queries <- replicateM q readLn
    putStr $ unlines $ map show $ solve childrenList queries
