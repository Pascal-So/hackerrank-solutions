import Control.Monad

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f lst = (takeWhile f lst, dropWhile f lst)

isBST :: [Int] -> Bool
isBST [] = True
isBST (root:lst) =
    let
        (smaller, bigger) = splitWhile (<root) lst
        biggerOk = all (>root) bigger
    in
        biggerOk && isBST smaller && isBST bigger

yesno :: Bool -> String
yesno False = "NO"
yesno True = "YES"

main = do
    t <- readLn
    replicateM_ t $ do
        dump <- getLine
        nums <- fmap (map read . words) getLine
        putStrLn $ yesno $ isBST nums
