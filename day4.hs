import Data.List
import Data.Maybe

type Interval = (Int, Int)

-- determine whether two lists fully overlap
fullyOverlap :: Eq a => [a] -> [a] -> Bool
fullyOverlap xs ys = xs `intersect` ys == xs || xs `intersect` ys == ys

-- determine whether two lists partly overlap
partlyOverlap :: Eq a => [a] -> [a] -> Bool
partlyOverlap xs ys = xs `intersect` ys /= []

-- parse a line
parseLine :: String -> (String, String)
parseLine = splitOn ','

-- parse a pair
parsePair :: String -> Interval
parsePair = toInts . splitOn '-'
  where toInts = \(x, y) -> (read x, read y)

-- split a string on a character into a pair
splitOn :: Char -> String -> (String, String)
splitOn c xs = (take i xs, drop (i + 1) xs)
  where i = fromJust $ elemIndex c xs

-- parse the input
parse :: String -> [(Interval, Interval)]
parse xs = map (\(x, y) -> (parsePair x, parsePair y)) $ map (parseLine) $ filter (\l -> l /= "") $ lines xs

-- convert an interval to a list
intervalToList :: Interval -> [Int]
intervalToList (s, e) = [s..e]

-- check if two elves have overlapping ranges
intervalsFullyOverlap :: (Interval, Interval) -> Bool
intervalsFullyOverlap (x, y) = fullyOverlap (intervalToList x) (intervalToList y)

-- check if two elves have overlapping ranges
intervalsPartlyOverlap :: (Interval, Interval) -> Bool
intervalsPartlyOverlap (x, y) = partlyOverlap (intervalToList x) (intervalToList y)

-- count the number of range overlaps given some input and predicate
overlapsBy :: ((Interval, Interval) -> Bool) -> String -> Int
overlapsBy f = length . filter (== True) . map f . parse

-- count the number of full overlaps in assignments
part1 :: String -> IO Int
part1 fileName = do contents <- readFile fileName
                    let os = overlapsBy intervalsFullyOverlap contents
                    return os

-- count the number of partial overlaps in assignments
part2 :: String -> IO Int
part2 fileName = do contents <- readFile fileName
                    let os = overlapsBy intervalsPartlyOverlap contents
                    return os