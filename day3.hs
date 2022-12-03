import Data.Char
import Data.List

-- calculate the priority for an item
priority :: Char -> Int
priority c | code >= 65 && code <= 90 = code - 65 + 26 + 1
           | code >= 97 && code <= 122 = code - 97 + 1
           | otherwise = error "invalid char"
  where code = ord c

-- compare two tuples for priority
priorityCompare :: (Char, Int) -> (Char, Int) -> Ordering
priorityCompare (_, p1) (_, p2) = compare p1 p2

-- find the item that appears in both lists
common :: Eq a => [a] -> [a] -> a
common xs ys = head $ intersect xs ys

-- find the item that appears in all 3 lists
commonInGroup :: Eq a => [a] -> [a] -> [a] -> a
commonInGroup as bs cs = head $ intersect (as) (intersect bs cs)

-- split the items into two "compartments"
split :: [a] -> ([a], [a])
split ss = (take c ss, drop c ss)
  where c = length ss `div` 2

-- group the bags into groups of elves
-- there are three elves in each group
groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups c xs = [take c xs] ++ groups c (drop c xs)

-- part 1
-- sum the priorities of the common items in each compartment for each bag
sumOfPriorities :: String -> IO Int
sumOfPriorities fileName = do contents <- readFile fileName
                              let ls = filter (/= "") $ lines contents
                              let commonItems = map ((uncurry common) . split) ls
                              let priorities = map (\i -> (i, priority i)) commonItems
                              let total = sum $ map snd priorities
                              return total

-- part 2
-- group the elves into threes and then find the common item in the group, then find the priorities and sum
sumOfBadgePriorities :: String -> IO Int
sumOfBadgePriorities fileName = do  contents <- readFile fileName
                                    let ls = filter (/= "") $ lines contents
                                    let gs = groups 3 ls
                                    let bs = map (\(as:bs:cs:_) -> commonInGroup as bs cs) gs
                                    let priorities = map (\i -> (i, priority i)) bs
                                    let total = sum $ map snd priorities
                                    return total