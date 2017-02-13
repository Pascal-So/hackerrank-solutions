import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative

data Turn = PLeft | PRight
type Path = [Turn]

data Node = Node Bool Node Node | Leaf

instance Show Node where
    show Leaf = ""
    show (Node s Leaf Leaf) = if s then "X" else "."
    show (Node s a b) = "(" ++ (show a) ++ " " ++ (if s then "X" else ".") ++ " " ++ (show b) ++ ")"    

readPath :: String -> Path
readPath str =
  let
    convert x = if x == '<' then PLeft else PRight
  in
    map convert $ tail $ init str

readQuery :: String -> (Int, Path)
readQuery str =
  let
    w = words str
    s = read $ head w
    path = readPath $ last w
  in
    (s, path)
  
pState :: ReadP Bool
pState = do
  let on = 'X'
  let off = '.'
  c <- satisfy (== off) <|> satisfy (== on)
  return (c == on)

pLeaf :: ReadP Node
pLeaf = do
  state <- pState
  return $ Node state Leaf Leaf

pBranch :: ReadP Node
pBranch = do
  satisfy (== '(')
  left <- pNode
  state <- pState
  right <- pNode
  satisfy (== ')')
  return $ Node state left right

pNode :: ReadP Node
pNode = pBranch <|> pLeaf

parseTree :: String -> Node
parseTree = fst . head . readP_to_S pNode

intToBase2 :: Int -> [Bool]
intToBase2 0 = [False]
intToBase2 1 = [True]
intToBase2 n = intToBase2 (n `div` 2) ++ intToBase2 (n `mod` 2)
  
base2toInt :: [Bool] -> Int
base2toInt states =
  let
    b2tI (x:xs) = (if x then 1 else 0) + 2 * (b2tI xs)
    b2tI [] = 0
  in
    b2tI $ reverse states


leftPad :: Int -> a -> [a] -> [a]
leftPad len element input =
  let
    remaining = len - (length input)
    pad = replicate remaining element
  in
    pad ++ input

getState :: Node -> Path -> Bool
getState (Node s _ _) [] = s
getState (Node _ l _) (PLeft:xs) = getState l xs
getState (Node _ _ r) (PRight:xs) = getState r xs

peekState :: Node -> Bool
peekState (Node s _ _) = s
peekState Leaf = False

getOutcome :: [Bool] -> [Bool] -> Bool
getOutcome rules states =
  let
    pos = base2toInt $ map not states
  in
    rules !! pos

nextIteration :: [Bool] -> Node -> Node
nextIteration rules tree =
  let
    ni (Node s l r) upperState =
      let
        leftState = peekState l
        rightState = peekState r
        states = [upperState, leftState, s, rightState]
        newState = getOutcome rules states
      in
        Node newState (ni l s) (ni r s)
    ni Leaf _ = Leaf
  in
    ni tree False

solveQuery :: [Node] -> Int -> Int -> IO ()
solveQuery trees queriesLeft currentPos = do
  (steps, path) <- fmap readQuery getLine
  let newPos = currentPos + steps
  let result = getState (trees !! newPos) path
  putStrLn $ if result then "X" else "."
  let newQueriesLeft = queriesLeft - 1
  if newQueriesLeft > 0 then
    solveQuery trees newQueriesLeft newPos
  else
    return ()

main = do
  rules <- fmap (leftPad 16 False . intToBase2) readLn
  initialTree <- fmap (parseTree . filter (/= ' ')) getLine
  
  let trees = iterate (nextIteration rules) initialTree

  nrQueries <- readLn
  solveQuery trees nrQueries 0
    
    
