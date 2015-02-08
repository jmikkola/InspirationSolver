

import Data.List (permutations)

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy a b = (mod a b) == 0

data Operator = Plus | Minus | Times | Divide
              deriving (Eq, Show)

data Trial = Trial [Operator] [Int]
           deriving (Eq, Show)

apply :: Operator -> Int -> Int -> Maybe Int
apply Plus a b  = Just (a + b)
apply Minus a b = Just (a - b)
apply Times a b = Just (a * b)
apply Divide a b = if isDivisibleBy a b then Just (div a b) else Nothing

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
combinations n items = if n < 2
                       then map list items
                       else concatMap (concatEach items) (combinations (n - 1) items)


operatorCombinations = combinations 3 allOperators

allOrderings :: [Int] -> [[Int]]
allOrderings = permutations

allTrials :: [Int] -> [Trial]
allTrials nums = [Trial ops num | ops <- operatorCombinations, num <- allOrderings nums]

find :: [Trial] -> Int -> Maybe Trial
find []     _ = Nothing
find (t:ts) n = case computeTrial t of
  Nothing -> find ts n
  Just r  -> if r == n then Just t else find ts n

compute :: Int -> [Int] -> Maybe Trial
compute n nums = find (allTrials nums) n

--main = putStrLn $ show $ compute 3 [1,3,4,7]
main = putStrLn $ show $ compute 4 [1,2,3,5]
