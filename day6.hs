import Data.List

-- get the groups from the buffer
charsets :: Int -> String -> [(Int, String)]
charsets n s = filter (\(_, s) -> length s >= n) $ charsets' n n s

-- get groups from a buffer and keep track of which group we are at
charsets' :: Int -> Int -> String -> [(Int, String)]
charsets' _ _ [] = []
charsets' i n b@(c:cs) = (i, take n b):(charsets' (i + 1) n cs)

-- find the first unique one
firstUniqueBy :: (Eq a, Eq b) => (a -> [b]) -> [a] -> Maybe a
firstUniqueBy _ [] = Nothing
firstUniqueBy f (a:as) | length a' == length (nub a') = Just a
                       | otherwise = firstUniqueBy f as
  where a' = f a

-- get the first unique n-length string from the buffer
-- or Nothing if there isn't one
firstUniqueFromBuf :: Int -> String -> Maybe (Int, String)
firstUniqueFromBuf n xs = firstUniqueBy (snd) $ charsets n xs

-- get the first unique string given a certain length
firstLengthUnique :: Int -> String -> IO (Maybe (Int, String))
firstLengthUnique n fileName = do
  cs <- readFile fileName
  let ls = lines cs
  let l = head ls
  let fu = firstUniqueFromBuf n l
  return fu -- >:)

-- main method because why not
main :: IO ()
main = do
  putStrLn "Enter the length of the buffer"
  n' <- getLine
  let n = read n' :: Int
  putStrLn "Enter the name of the file you want to check"
  f <- getLine
  fu <- firstLengthUnique n f
  print fu
  return ()