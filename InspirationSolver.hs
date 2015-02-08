import Data.List (permutations)
import System.IO

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy _ 0 = False
isDivisibleBy a b = (mod a b) == 0

data Operator = Plus | Minus | Times | Divide
              deriving (Eq)

instance Show Operator where
  show Plus   = "+"
  show Minus  = "-"
  show Times  = "*"
  show Divide = "/"

data Trial = Trial [Operator] [Int]
           deriving (Eq)

instance Show Trial where
  show (Trial ops nums) = unwords [
    "(", show (nums !! 0), show (ops !! 0), show (nums !! 1), ")",
    show (ops !! 1),
    "(", show (nums !! 2), show (ops !! 2), show (nums !! 3), ")"
    ]

apply :: Operator -> Int -> Int -> Maybe Int
apply Plus   a b = Just (a + b)
apply Minus  a b = Just (a - b)
apply Times  a b = Just (a * b)
apply Divide a b = if isDivisibleBy a b
                   then Just (div a b)
                   else Nothing

computeTrial :: Trial -> Maybe Int
computeTrial (Trial ops nums) = do
  left <- apply (ops !! 0) (nums !! 0) (nums !! 1)
  right <- apply (ops !! 2) (nums !! 2) (nums !! 3)
  apply (ops !! 1) left right

allOperators = [Plus, Minus, Times, Divide]

list a = [a]

concatEach :: [a] -> [a] -> [[a]]
concatEach concats to = map (\c -> c : to) concats

combinations :: Int -> [a] -> [[a]]
combinations n items =
  if n < 2
  then map list items
  else concatMap (concatEach items) (combinations (n - 1) items)

operatorCombinations = combinations 3 allOperators

allOrderings :: [Int] -> [[Int]]
allOrderings = permutations

allTrials :: [Int] -> [Trial]
allTrials nums = [
  Trial ops num
  | ops <- operatorCombinations, num <- allOrderings nums
  ]

find :: [Trial] -> Int -> Maybe Trial
find []     _ = Nothing
find (t:ts) n = case computeTrial t of
  Nothing -> find ts n
  Just r  -> if r == n then Just t else find ts n

compute :: Int -> [Int] -> Maybe Trial
compute n nums = find (allTrials nums) n

main = do
  putStr "Number to create: "
  hFlush System.IO.stdout
  line <- getLine
  let n = read line
  putStr "Numbers to work with: "
  hFlush System.IO.stdout
  line <- getLine
  let nums = map read (words line)
  putStrLn $ show $ compute n nums
