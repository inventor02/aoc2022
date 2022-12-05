-- represent crates as characters, with the topmost item being at the start of the list
type Crate = [Char]

-- instructions - move x crates from crate a to crate b
data Instr = Move Int Int Int deriving (Show)

-- split input up into crates and instructions
splitInput :: [String] -> ([String], [String])
splitInput = (\(xs, ys) -> (xs, tail ys)) . break (== "")

-- parse all instructions
-- ignore empty lines
parseInstructions :: [String] -> [Instr]
parseInstructions = map parseInstruction . filter (/= "")

-- parse an instruction from a string
parseInstruction :: String -> Instr
parseInstruction [] = error "no instruction"
parseInstruction ss = Move count from to
  where ws = words ss
        ws' = [ e | (e, i) <- zip ws [0..], i `mod` 2 /= 0 ]
        (count:from:to:[]) = map (read) ws'

-- parse crates from some lines
parseCrates :: [String] -> [Crate]
parseCrates ls = map (filter (/= ' ')) . map (filter (/= '_')) .  map reverse $ foldl addCharsToCrates [] ls'
  where ls' = filter (\(_:e:_) -> e /= '1') $ map parseCharsFromLine ls

-- parse the crate characters from the line
parseCharsFromLine :: String -> String
parseCharsFromLine = filter (/= '[') . filter (/= ']') . repl . dropSeps
  where repl (' ':' ':' ':cs) = repl ("[_]" ++ cs)
        repl (a:b:c:cs) = a:b:c:(repl cs)
        repl [] = []
        dropSeps = map snd . filter (\(i, _) -> i `mod` 4 /= 0) . zip [1..]

-- add characters to the top of each crate in turn
addCharsToCrates :: [Crate] -> [Char] -> [Crate]
addCharsToCrates cs [] = cs
addCharsToCrates [] (x:xs) = (x:[]):(addCharsToCrates [] xs)
addCharsToCrates (c:cs) (x:xs) = (x:c):(addCharsToCrates cs xs)

-- run all instructions on the crates
runAll :: [Instr] -> [Crate] -> [Crate]
runAll [] cs = cs
runAll (i:is) cs = runAll is (exec i cs)

runAll' :: [Instr] -> [Crate] -> [Crate]
runAll' [] cs = cs
runAll' (i:is) cs = runAll' is (exec' i cs)

-- perform an instruction on the crates
exec :: Instr -> [Crate] -> [Crate]
exec (Move 0 _ _) cs = cs
exec (Move c from to) cs = exec (Move (c - 1) from to) (move from to cs)
  where move :: Int -> Int -> [Crate] -> [Crate]
        move from to = replace (to - 1) (elem:to') . replace (from - 1) (drop 1 from')
          where from' = cs !! (from - 1)
                to' = cs !! (to - 1)
                elem = head from'

-- perform an instruction on the crates
exec' :: Instr -> [Crate] -> [Crate]
exec' (Move c from to) cs = move from to cs
  where move :: Int -> Int -> [Crate] -> [Crate]
        move from to = replace (to - 1) (elems ++ to') . replace (from - 1) (drop c from')
          where from' = cs !! (from - 1)
                to' = cs !! (to - 1)
                elems = take c from'

-- performs an in-place replacement of an element at an index in a list
replace :: Int -> a -> [a] -> [a]
replace i x es = map (\(r, e) -> repl r e x) es''
  where es' = zip [0..] es
        es'' = map (\(i', e) -> (i == i', e)) es'
        repl :: Bool -> a -> a -> a
        repl False o _ = o
        repl _ _ x = x

-- get the element at the top of each crate
tops :: [Crate] -> String
tops = map head

-- part 1
runInstructions :: String -> IO String
runInstructions fileName = do contents <- readFile fileName
                              let ls = lines contents
                              let (cs, is) = (splitInput ls)
                              let (crates, instrs) = (parseCrates cs, parseInstructions is)
                              let result = tops $ runAll instrs crates
                              return result

-- part 2
runInstructions' :: String -> IO String
runInstructions' fileName = do contents <- readFile fileName
                               let ls = lines contents
                               let (cs, is) = (splitInput ls)
                               let (crates, instrs) = (parseCrates cs, parseInstructions is)
                               let result = tops $ runAll' instrs crates
                               return result