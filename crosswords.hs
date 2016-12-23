-- full score solution

import Control.Monad
import Data.List
import Data.Function
import Data.Tuple

type Point = (Int, Int)
type Board = [String]
type Slot = (Point, Int, Direction)
data Direction = Horizontal | Vertical deriving (Eq, Show)

findSlots :: String -> [(Int, Int)] -- (start, length), where start is 0-indexed
findSlots s =
    let
        grp = groupBy ((==) `on` fst) $ zip s [0..]
        slots = filter (\x -> (fst $ head x) == '-') grp
        lslots = filter (\x -> length x > 1) slots -- don't include length 1
    in
        map (\x -> ((snd $ head x), length x)) lslots

findWordSlots :: Board -> [(Point, Int)]
findWordSlots board =
    let
        slots = map findSlots board
    in    
        concatMap (\(y, lst) -> map (\(x, len) -> ((y,x),len)) lst) $ zip [0..] slots


allSlots :: Board -> [Slot]
allSlots board = 
    let 
        slotsx = findWordSlots board
        slotsy = findWordSlots $ transpose board
        slotsxDir = map (\(a,b) -> (a,b,Horizontal)) slotsx
        slotsyDir = map (\(a,b) -> (swap a,b,Vertical)) slotsy
    in
        slotsxDir ++ slotsyDir


splitOn :: Char -> String -> [String]
splitOn c = 
    filter (/= [c]) . groupBy (\a b -> not $ c `elem` [a,b])
    
mapIndex :: (a->a) -> Int -> [a] -> [a]
mapIndex _ n [] = error "Out of range error when mapping on index"
mapIndex f n (x:xs) =
    if n < 0 then
        error "Negative index when mapping on index"
    else if n == 0 then
        (f x) : xs
    else
        x:(mapIndex f (n-1) xs)

insertInRow :: Int -> String -> String -> String
insertInRow pos new base =
    let
        start = take pos base
        endpos = pos + length new
        end = drop endpos base
    in
        start ++ new ++ end

insertInBoard :: String -> Slot -> Board -> Board
insertInBoard str (point, len, Vertical) =
    transpose . insertInBoard str ((swap point), len, Horizontal) . transpose
insertInBoard str ((y,x), _, Horizontal) =
    mapIndex (insertInRow x str) y

extractFromBoard :: Slot -> Board -> String
extractFromBoard (p, len, Vertical) board =
    extractFromBoard ((swap p), len, Horizontal) . transpose $ board
extractFromBoard ((y,x), len, Horizontal) board =
    take len $ drop x $ board!!y
    

matchStrings :: String -> String -> Bool
matchStrings needle pattern =
    not $ any (\(a,b) -> a /= b && b /= '-') $ zip needle pattern



solveCrosswords :: [Slot] -> [String] -> Board -> Board
solveCrosswords _ [] board = board
solveCrosswords slots (w:ws) board =
    let
        lSlots = filter (\(_, len, _) -> len == length w) slots
        fittingSlots = filter (\slot -> matchStrings w $ extractFromBoard slot board) lSlots
    in
        if null fittingSlots then -- can't insert w in to Board
            []
        else
            let 
                res = dropWhile null $ map (\x -> solveCrosswords (slots \\ [x]) ws $ insertInBoard w x board) slots
            in
                if (not.null) res then
                    head res
                else
                    []
                

main = do
    board <- replicateM 10 getLine
    words_tmp <- getLine
    let wrds = splitOn ';' words_tmp
    putStrLn $ unlines $ solveCrosswords (allSlots board) wrds board
