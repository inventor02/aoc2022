{-# LANGUAGE LambdaCase #-}
-- WNWW                                       WNXXW  
-- XKKXNW   WWW                               WX00KNW
-- N00K0XNWNXKKN            W             WWWW N0O0KN
-- WX0O0KKK000XW       WNNWNNKKNW         NKKXWN0O00X
--  W0OOO0KK0XW     WX0kkKXXK0kkO0XNW     WX0KXX00KKX
--   N0O0KKKKX    WX0xdxOKKKKK0OkOO00KNW   X00000OO0X
--   WX00KK00N  WXOkkkkxkO0OOOkkkkkkkOO0NWWXOO00OO0KN
--    WNK0KKNWWN0OkkOOkxol::::cldkOOOOOOOXWWXKK00KNW 
--      WWNW  N0OOOOOx:'.........;okOOOOOOXW WWWW    
--           WKOOOOkl'.............:xOOOOO0N         
--           NK0OOkl...          ...;xOOOO0XW        
--          WX00OOo'..           ....:kOOO0KW        
--          WX0OOx;...   ..'''.   ....lkOO0KW        
--           X0Okl.....,lxO00Oko;.....,dOOOXW        
--           NKOx;...,oO0KKKKKK0Od;....ckO0N         
--           WXOo,..:x000000000000kc...:xOKW         
--            WKkocokOOO0000000000Okdc:ok0N          
--             NKKOkkOOOOOOOOOOOOOOOkkO00XW          
--              WWNXKOOkkOOOOOOOOOOO0KXNNW           
--                  WWNXKKK0000KKXNNW              

-- cursed M-way tree representing the filesystem
data Filesystem = File String Int | Dir String [Filesystem]
  deriving (Eq, Show, Read)

-- we use a zipper to navigate around the filesystem
data Movement = D String [Filesystem]
  deriving (Eq, Show, Read)
type Trail = [Movement]
type Zipper = (Filesystem, Trail)

-- file name, file size
type File' = (String, Int)

data TerminalCommand = CD String | LS [String] deriving (Show)

-- go to a directory
goDir :: String -> Zipper -> Zipper
goDir _ (File _ _, _) = error "can't go to a directory from a file"
goDir x (Dir dn fs, ts) =
  let goDir' :: String -> [Filesystem] -> [Filesystem] -> (Filesystem, [Filesystem])
      goDir' _ _ [] = error "can't find the target"
      goDir' x as (f@(File _ _):ds) = goDir' x (f:as) ds
      goDir' x as (d@(Dir dn fs):ds) | dn == x = (d, as ++ ds)
                                     | otherwise = goDir' x (d:as) ds
      td = goDir' x [] fs
  in (fst td, D dn (snd td):ts)

-- go to the root of the filesystem
root :: Zipper -> Zipper
root z@(_, []) = z
root z@(_, _:_) = root $ goUp z

-- go up one level
goUp :: Zipper -> Zipper
goUp (_, []) = error "at the root"
goUp (cfs, (D dn ods):ts) = (Dir dn (cfs:ods), ts)

dirContains :: (String -> Filesystem -> Bool) -> String -> Filesystem -> Bool
dirContains f _ (File _ _) = error "files cannot have kids"
dirContains f x (Dir _ ds) = (>0) $ length $ filter (f x) ds

-- return whether a directory contains another directory
dirContainsDir :: String -> Filesystem -> Bool
dirContainsDir = dirContains fil
  where fil :: String -> Filesystem -> Bool
        fil _ (File _ _) = False
        fil x (Dir n _) = x == n

-- return whether a directory contains another directory
dirContainsFile :: String -> Filesystem -> Bool
dirContainsFile = dirContains fil
  where fil :: String -> Filesystem -> Bool
        fil x (File n _) = x == n
        fil _ (Dir _ _) = False

-- make a directory
mkdir :: String -> Zipper -> Zipper
mkdir n (Dir dn ds, ts) = (Dir dn (Dir n []:ds), ts)

-- make a file
touch :: String -> Int -> Zipper -> Zipper
touch n s (Dir dn ds, ts) = (Dir dn (File n s:ds), ts)

-- change to a directory
cd :: String -> Zipper -> Zipper
cd d z@(fs@(Dir n ds), ts) | dirContainsDir d fs = goDir d (fs, ts)
                           | otherwise = goDir d $ mkdir d z

-- ensure that the directory exists
ensureDir :: String -> Zipper -> Zipper
ensureDir dn z@(d, ts) | dirContainsDir dn d = z
                       | otherwise = mkdir dn z

-- ensure that the files that are listed exist
ensureFiles :: [File'] -> Zipper -> Zipper
ensureFiles _ (File _ _, _) = error "cannot have children on files"
ensureFiles [] z = z
ensureFiles (f:fs) z@(d, _) | dirContainsFile (fst f) d = ensureFiles fs z
                            | otherwise = ensureFiles fs (uncurry touch f z)

-- parse the input lines
parseLines :: [String] -> [TerminalCommand]
parseLines [] = []
parseLines ls = tc:parseLines rs
  where b = head ls:ls'
        ls' = takeWhile (\case
                          [] -> False
                          (f:_) -> f /= '$') (drop 1 ls)
        rs = drop (length b) ls
        cmd = words (head b) !! 1
        args = drop 2 $ words $ head b
        tc  | cmd == "cd" = CD (head args)
            | cmd == "ls" = LS (drop 1 b)
            | otherwise = error "not a valid command"

-- simulate running some commands on a zipper
simulateCommands :: [TerminalCommand] -> Zipper -> Zipper
simulateCommands [] z = z
simulateCommands ((CD "/"):cs) z = simulateCommands cs (root z)
simulateCommands ((CD d):cs) z = simulateCommands cs (goDir d $ ensureDir d z)
simulateCommands ((LS fs):cs) z = simulateCommands cs (ensureFiles fs' z)
  where fs' = map (\f -> (head (words f), read (words f !! 1))) fs

-- execute part 1 of the challenge
part1 :: String -> IO ()
part1 fileName = do
  contents <- readFile fileName
  let cmds = parseLines $ filter (/= "") $ lines contents
  print cmds
  return ()