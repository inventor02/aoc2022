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

-- go to a directory
goDir :: String -> Zipper -> Zipper
goDir _ (File _ _, _) = error "can't go to a directory from a file"
goDir x (Dir dn fs, ts) =
  let goDir' :: String -> [Filesystem] -> [Filesystem] -> (Filesystem, [Filesystem])
      goDir' _ _ []= error "can't find the target"
      goDir' x as (f@(File _ _):ds) = goDir' x (f:as) ds
      goDir' x as (d@(Dir dn fs):ds) | dn == x = (d, as ++ ds)
                                     | otherwise = goDir' x (d:as) ds
      td = goDir' x [] fs
  in (fst td, (D dn (snd td)):ts)

-- go up one level
goUp :: Zipper -> Zipper
goUp (_, []) = error "at the root"
goUp (cfs, (D dn ods):ts) = (Dir dn (cfs:ods), ts)

-- return whether a directory contains another directory
dirContainsDir :: String -> Filesystem -> Bool
dirContainsDir _ (File _ _) = error "files cannot have kids"
dirContainsDir x (Dir _ ds) = (>0) $ length $ filter (fil x) ds
  where fil :: String -> Filesystem -> Bool
        fil _ (File _ _) = False
        fil x (Dir n _) = x == n

-- make a directory
mkdir :: String -> Zipper -> Zipper
mkdir n (Dir dn ds, ts) = (Dir dn ((Dir n []):ds), ts)

-- make a file
touch :: String -> Int -> Zipper -> Zipper
touch n s (Dir dn ds, ts) = (Dir dn ((File n s):ds), ts)

-- change to a directory
cd :: String -> Zipper -> Zipper
cd d z@(fs@(Dir n ds), ts) | dirContainsDir d fs = goDir d (fs, ts)
                           | otherwise = goDir d $ mkdir d z

ensureFiles :: [String] -> Zipper -> Zipper
ensureFiles [] z = z
ensureFiles (f:fs) z = undefined