data Move = Rock | Paper | Scissors deriving (Eq, Read, Show)
data Outcome = Win | Draw | Lose deriving (Show)

-- opponent move, our move
type Round = (Move, Move)
type IdealRound = (Move, Outcome)

parseMove :: Char -> Move
parseMove 'A' = Rock
parseMove 'B' = Paper
parseMove 'C' = Scissors
parseMove 'X' = Rock
parseMove 'Y' = Paper
parseMove 'Z' = Scissors
parseMove _ = error "invalid move"

parseOutcome :: Char -> Outcome
parseOutcome 'X' = Lose
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win

parse :: String -> Round
parse (o:' ':p:[]) = (parseMove o, parseMove p)
parse _ = error "invalid round"

parse' :: String -> IdealRound
parse' (o:' ':p:[]) = (parseMove o, parseOutcome p)
parse' _ = error "invalid round"

shapeScore :: Move -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

play :: Move -> Move -> Outcome
play Rock Paper = Win
play Paper Scissors = Win
play Scissors Rock = Win
play a b | a == b = Draw
play _ _ = Lose

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore _ = 0

makeWin :: Move -> Move
makeWin Rock = Paper
makeWin Paper = Scissors
makeWin Scissors = Rock

makeDraw :: Move -> Move
makeDraw m = m

makeLoss :: Move -> Move
makeLoss Rock = Scissors
makeLoss Paper = Rock
makeLoss Scissors = Paper

makeOutcome :: Outcome -> Move -> Move
makeOutcome Win = makeWin
makeOutcome Draw = makeDraw
makeOutcome Lose = makeLoss

score :: Round -> Int
score (o, p) = shapeScore p + (outcomeScore $ play o p)

score' :: IdealRound -> Int
score' (m, o) = score (m, p)
  where p = makeOutcome o m

finalScore :: String -> IO Int
finalScore file = do  contents <- readFile file
                      let rounds = map (parse) $ filter (\l -> l /= "") $ lines contents
                      print $ (show $ length rounds) ++ " rounds"
                      let scores = map (score) rounds
                      print $ (show $ length scores) ++ " scores"
                      print $ zip rounds scores
                      let total = sum scores
                      return total

finalScore' :: String -> IO Int
finalScore' file = do contents <- readFile file
                      let rounds = map (parse') $ filter (\l -> l /= "") $ lines contents
                      print $ (show $ length rounds) ++ " rounds"
                      let scores = map (score') rounds
                      print $ (show $ length scores) ++ " scores"
                      print $ zip rounds scores
                      let total = sum scores
                      return total