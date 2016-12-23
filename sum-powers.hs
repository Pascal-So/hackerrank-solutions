import Data.List

data Map a = Map [(Int, a)]

instance Functor (Map a) where
  fmap f (Map m) = Map $ map f m

combineWith :: (a -> a -> a) -> Map a -> Map a -> Map a
combineWith f x (Map ins) =
  foldl (insertWith f) x ins

insertWith :: (a -> a -> a) -> Map a -> (Int, a) -> Map a
insertWith f (Map lst) (k, v) =
  let
    existing = filter (\x -> fst x == k) lst
    new = 
        if existing == [] then
            (k,v)
        else
            (k, f (snd $ head existing) v)
  in
    Map $ new : (lst \\ existing)

lookupM :: Int -> Map a -> Maybe a
lookupM k (Map lst) =
  lookup k lst

solvePows :: Int -> Int -> Int
solvePows x n =
  let
    pows = takeWhile (<x) $ map (^n) [1..]
  in
    maybe 0 id $ lookupM x $ foldl (\acc x -> combineWith (+) acc $ map ((a,b) -> (a+x, b)) acc) (Map []) pows

main = do
  x <- readLn
  n <- readLn
  putStrLn $ show $ solvePows x n
