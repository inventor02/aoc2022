import Data.Text (splitOn, pack, unpack)
import Data.List (sortBy)

topThreeTotal :: String -> IO Int
topThreeTotal fileName = do topThree <- calCount fileName
                            let tot = foldl (+) 0 $ map (fst) topThree
                            return tot

calCount :: String -> IO [(Int, Int)]
calCount fileName = do contents <- readFile fileName
                       let lines = map (unpack) $ splitOn (pack "\n") (pack contents)
                       let elves = splitToElves lines
                       let elfTotals = totals elves
                       let elfTotals' = zip elfTotals [1..]
                       return (take 3 $ sortBy (\(x, _) (y, _) -> compare y x) elfTotals')

splitToElves :: [String] -> [[String]]
splitToElves s = case dropWhile p s of
                      [] -> []
                      s' -> w : splitToElves s''
                            where (w, s'') = break p s'
  where p = \s -> s == ""

totals :: [[String]] -> [Int]
totals es =
  let toInts :: [String] -> [Int]
      toInts [] = []
      toInts (s:ss) = read s : toInts ss
      total :: [Int] -> Int
      total is = foldl (+) 0 is
  in map (total . toInts) es