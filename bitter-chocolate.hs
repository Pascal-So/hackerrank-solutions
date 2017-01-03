import qualified Data.IntMap as IM
import Control.Monad

data GameOutcome = Win | Lose deriving Eq

instance Show GameOutcome where
  show Win = "WIN"
  show Lose = "LOSE"

type Shape = [Int]
type Pos = (Int, Int)

maxBarLen :: Int
maxBarLen = 26

numToShape :: Int -> Shape
numToShape n =
  let
    b1 = maxBarLen
    b2 = b1 * maxBarLen
    (d2, r2) = divMod n b2
    (d1, d0) = divMod r2 b1
  in
    [d0, d1, d2]

shapeToNum :: Shape -> Int
shapeToNum [d0, d1, d2] =
  let
    b1 = maxBarLen
    b2 = b1 * maxBarLen
  in
    d0 + d1*b1 + d2*b2

cutShape :: Pos -> Shape -> Shape
cutShape (y,x) s =
  let
    start = take y s
    end = map (min x) $ drop y s
  in
    start ++ end
  
getSubshapes :: Shape -> [Shape]
getSubshapes s =
  let
    z = zip s [0..]
    coords = concatMap (\(row, y) -> map (\x -> (y,x)) [0..row-1]) z
  in
    map (\p -> cutShape p s) coords

getOutcome :: IM.IntMap GameOutcome -> Shape -> GameOutcome
getOutcome m s =
  let
    subshapes = getSubshapes s
    reachesLose = any (==Lose) $ map (\sub -> IM.findWithDefault Win (shapeToNum sub) m) subshapes
  in
    if reachesLose then Win else Lose

buildMap :: IM.IntMap GameOutcome
buildMap =
  let
    startMap = IM.singleton 0 Win -- construct IntMap with single entry
    allShapes = [[a,b,c] | a<-[0..maxBarLen-1], b<-[0..maxBarLen-1], c<-[0..maxBarLen-1], a>=b, b>=c, a>0]
  in
    foldl (\accMap shape -> IM.insert (shapeToNum shape) (getOutcome accMap shape) accMap) startMap allShapes

solve :: [Shape] -> [String]
solve shapes =
  let
    m = buildMap
  in
    map (show.getOutcome m) shapes

main = do
  n <- readLn
  tmp <- replicateM n getLine
  let shapes = fmap (map read.words) tmp
  putStr $ unlines $ solve shapes
