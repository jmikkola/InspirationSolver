import Data.List (find, permutations)
import Data.Maybe (mapMaybe)
import System.IO

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy _ 0 = False
isDivisibleBy a b = (mod a b) == 0

class Render a where
  render :: a -> String

data Operator = Plus | Minus | Times | Divide
              deriving (Eq, Show)

instance Render Operator where
  render Plus   = "+"
  render Minus  = "-"
  render Times  = "*"
  render Divide = "/"

allOperators = [Plus, Minus, Times, Divide]

data ExprTree = Val Int
              | Op Operator ExprTree ExprTree
              deriving (Eq, Show)

renderList :: ExprTree -> [String] -> [String]
renderList (Val n)    items = (show n) : items
renderList (Op o l r) items = renderList l (renderList r (render o : items))

instance Render ExprTree where
  render expr = unwords $ renderList expr []

apply :: Operator -> Int -> Int -> Maybe Int
apply op a b = case op of
  Plus   -> Just $ a + b
  Minus  -> Just $ a - b
  Times  -> Just $ a * b
  Divide -> if isDivisibleBy a b
           then Just $ div a b
           else Nothing

evalExpr :: ExprTree -> Maybe Int
evalExpr (Val n)    = Just n
evalExpr (Op o l r) = do
  left <- evalExpr l
  right <- evalExpr r
  apply o left right

tails :: [a] -> [[a]]
tails []     = []
tails [_]    = []
tails (a:as) = as : (tails as)

partitions :: [a] -> [([a], [a])]
partitions items = zip (reverse $ tails $ reverse items) (tails items)

genOps :: ExprTree -> ExprTree -> [ExprTree]
genOps left right = map (\o -> Op o left right) allOperators

genExprs :: [Int] -> [ExprTree]
genExprs []   = []
genExprs [n]  = [Val n]
genExprs nums = do
  splits <- partitions nums
  genSub splits

genSub :: ([Int], [Int]) -> [ExprTree]
genSub (lnums, rnums) = do
  left <- genExprs lnums
  right <- genExprs rnums
  genOps left right

-- Generates 7680 expressions given 4 numbers
allExpressions :: [Int] -> [ExprTree]
allExpressions nums = do
  numOrderings <- permutations nums
  genExprs numOrderings

evalsTo :: Int -> ExprTree -> Bool
evalsTo n expr = case evalExpr expr of
  Nothing -> False
  Just r  -> r == n

-- Find the one that matches
findMatch :: Int -> [ExprTree] -> Maybe ExprTree
findMatch n = find (evalsTo n)

getExpression :: Int -> [Int] -> Maybe ExprTree
getExpression n nums = findMatch n (allExpressions nums)

showResult :: Maybe ExprTree -> String
showResult Nothing  = "No matches"
showResult (Just e) = render e

main = do
  putStr "Number to create: "
  hFlush System.IO.stdout
  line <- getLine
  let n = read line
  putStr "Numbers to work with: "
  hFlush System.IO.stdout
  line <- getLine
  let nums = map read (words line)
  putStrLn $ showResult $ getExpression n nums
