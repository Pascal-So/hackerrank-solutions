import Text.ParserCombinators.ReadP

data Turn = Left | Right
type Path = [Turn]

data Node = Node Bool Node Node | Leaf

readPath :: String -> Path
readPath str =
  let
    convert x = if x == '<' then Left else Right
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
parseTree = readP_to_S pNode

intToBase2 :: Int -> [Bool]
intToBase2 0 = [False]
intToBase2 1 = [True]
intToBase2 n = intToBase2 (n `div` 2) ++ intToBase2 (n `mod` 2)
  
base2toInt :: [Bool] -> Int
base2toInt states =
  let
    b2tI [] = 0
    b2tI [x:xs] = (if x then 1 else 0) + 2 * (b2tI xs)
  in
    b2tI $ reverse states

getState :: Node -> Path -> Bool
getState (Node s _ _) [] = s
getState (Node _ l _) (<:xs) = getState l xs
getState (Node _ _ r) (>:xs) = getState r xs

peekState :: Node -> Bool
peekState (Node s _ _) = s
peekState Leaf = False

getOutcome :: [Bool] -> [Bool] -> Bool
getOutcome rules states =
  let
    pos = base2ToInt $ map not states
  in
    rules !! pos

nextIteration :: [Bool] -> Node -> Node
nextIteration rules tree =
  let
    ni (Node s l r) upperState =
      let
        leftState = peekState l
        rightState = peekState r
        states = [upperState, leftState, rightState, s]
      in
        
    ni Leaf _ = Leaf

main = do
  rules <- fmap intToBase2 readLn
  initialTree <- fmap parseTree getLine
  let trees = iterate (nextIteration rules) initialTree
  nrQueries <- readLn
  
