-- import Debug.Trace(trace)
import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified System.Random as R (mkStdGen, randomRs)
import Data.List(sortBy)

type Die = Int
type Dice = (Die, Die)

-- Number of pieces in a triangle
type Chip = Int
data Side = White | Black
  deriving (Eq, Show)
-- Triangle with number and type of pieces
type Point = Maybe (Side, Chip)
type Points = [Point]

type Pos = Int
-- Move from to
data Move = Move Pos Pos
          | Enter Pos Pos
          | BearOff Pos Pos
  deriving (Eq, Show, Ord)
-- type Moves = [Move]

-- Either first dice throw to determine who starts
-- Or an action by player
data GameAction = PlayerAction Side PlayerDecision
                | InitialThrows Die Die
  deriving (Eq, Show)

-- Two play actions possible Move or Throw
data PlayerDecision = Moves [Move]
                    | Throw Dice
  deriving (Eq, Show)

-- Initial throw, 2 player decitions and game end
data GameState = PlayersToThrowInitial
               | ToMove Side Dice
               | ToThrow Side
               | GameFinished Side
  deriving (Eq, Show)

-- Error checks
data InvalidDecisionType = NoPieces Pos
                          | MovedOntoOpponentsClosedPoint Pos
                          | NoBarPieces Side
  deriving (Eq, Show)

-- Error checks
data InvalidAction = ActionInvalidForState GameState GameAction
                   | InvalidPlayerDecision Game PlayerDecision InvalidDecisionType
  deriving (Eq, Show)

-- triangles and chips, barWhite, barBlack
data Board
  = Board Points Int Int
  deriving (Eq, Show)

-- Storing game at every turn
data Game = Game { gameBoard :: Board,
                  gameActions :: [GameAction],
                  gameState :: GameState}
  deriving (Eq)

instance Show Game where
  show game = "Game Board: " ++ show board ++
              "\n\nGame Actions: " ++ show actions ++
              "\n\nGame State: " ++ show state where
    board = gameBoard game
    actions = if (length gActions >=6)
              then (show $ take 3 gActions) ++ ".........." ++ (show $ takeLast 3 gActions)
              else show gActions
    state = gameState game
    gActions = gameActions game

-- splitComm xs = split xs ','
--
-- split :: String -> Char -> [String]
-- split [] delim = [""]
-- split (c:cs) delim
--     | c == delim = "" : rest
--     | otherwise = (c : head rest) : tail rest
--     where
--         rest = split cs delim

-- Start with this board
initialBoard :: Board
initialBoard = Board [ Nothing, Just (White, 2), Nothing, Nothing, Nothing, Nothing, Just (Black, 5), Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Just (White, 5),
                      Just (Black, 5), Nothing, Nothing, Nothing, Just (White, 3), Nothing, Just (White, 5), Nothing, Nothing, Nothing, Nothing, Just (Black, 2), Nothing
                     ] 0 0

-- Can try bearing off with this board
-- Bear off Black 1 dice roll,  Black 2 dice roll,  Black 1,2 dice rolls
bearOffBoard :: Board
bearOffBoard = Board [ Nothing, Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
                      Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, 2), Nothing
                     ] 0 0

-- Board with black chips on bar
-- Call with dieroll 2
barBoard :: Board
barBoard = Board [ Nothing, Nothing, Just (Black, 3), Just (White, 3), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
                   Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
                   ] 0 2

-- Random Test Board
boardTest1 :: Board
boardTest1 = Board [ Nothing, Just (Black, 2), Just (Black, 5), Just (White, 1), Just (Black, 2), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
                     Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, 5), Nothing, Just (White, 2), Just (White, 5), Just (Black, 1), Just (White, 2), Nothing
                    ] 0 0

-- Test Board with elements in endzone
endBoard :: Board
endBoard = Board [ Nothing, Just (White,1),  Nothing, Nothing, Nothing, Nothing, Just (Black, 5), Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Just (White, 5),
                   Just (Black, 5), Nothing, Nothing, Nothing, Just (White, 3), Nothing, Just (White, 5), Nothing, Nothing, Nothing, Nothing, Just (Black, 2), Nothing
                  ] 0 0


allDiceRolls :: [Dice]
allDiceRolls = [(i,j) | i <- [1..6], j <- [i..6]]


--------------- Helper Functions ---------------
-- Take the last n elements from xs
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

newGame :: Game
newGame = Game initialBoard [] PlayersToThrowInitial

-- Black goes from 24 -> 1, white from 1 -> 24
direction :: Side -> Int
direction White = 1
direction Black = -1

-- Opposite sides
opposite :: Side -> Side
opposite White = Black
opposite Black = White

-- Get the chip at position from Board
getChip :: Board -> Pos -> Point
getChip (Board b _ _) pos = b !! (pos)

-- Returns False if there are any chips on any triangle for the given side
-- In the given points
checkChipSide :: [Point] -> Side -> Bool
checkChipSide [] _ = True
checkChipSide (p:pts) side = case p of
  -- If Nothing, then move on to next point
  Nothing -> checkChipSide pts side
  -- Otherwise, check if chips side == given side
  Just (s,_)  | s==side -> False
                  | otherwise -> checkChipSide pts side

dieList :: Dice -> [Die]
dieList (d1, d2) =
  if d1 == d2 then [d1, d1, d1, d1]
              else [d1, d2]

-- increase bar value
incBar :: Side -> Board -> Board
incBar White (Board b bw bb) = Board b (bw+1) bb
incBar Black (Board b bw bb) = Board b bw     (bb+1)

-- decrease bar value
decBar :: Side -> Board -> Either InvalidDecisionType Board
decBar side (Board b bw bb) | side==White && bw>0 = Right (Board b (bw-1) bb)
                            | side==Black && bb>0 = Right (Board b bw (bb-1))
                            | otherwise = Left (NoBarPieces White)



--------------- Functions ---------------
-- move chip from 'from' to 'to'
-- bear off handled by moving and removing the piece from the board
-- Enter (bar) handled by subtracting from bar
move :: Side -> Board -> Move -> Either InvalidDecisionType Board
move side board (Move from to) = handleMoves board side from to
move side board (BearOff from to) = handleBearOffMove (handleMoves board side from to) side
move side board (Enter from to) = takePiece from board side >>= landPiece to side

-- Handle regular moves
handleMoves :: Board -> Side -> Pos -> Pos -> Either InvalidDecisionType Board
handleMoves board side from to =
  case getChip board from of
    Just (_,_)  -> takePiece from board side >>= landPiece to side
    Nothing     -> Left (NoPieces from)

-- Handle Bear off moves
handleBearOffMove :: Either InvalidDecisionType Board -> Side -> Either InvalidDecisionType Board
handleBearOffMove board side =
  case board of
    Right (Board bd bw bb)  | side==Black -> Right (Board ([Nothing] ++ (tail bd)) bw bb)
                            | side==White -> Right (Board ((take 25 bd) ++ [Nothing]) bw bb)
    Right (Board _ _ _)     -> board
    Left err                -> Left err

-- take piece from the board, throws error on nonlegality
takePiece :: Pos -> Board -> Side -> Either InvalidDecisionType Board
takePiece (-1) board side = decBar side board
takePiece pos board@(Board b bw bb) _ =
  case getChip board pos of
    Just (s, n) -> Right (Board (take (pos) b ++ [dec1 s n] ++ drop (pos+1) b) bw bb)
    Nothing     -> Left (NoPieces pos)
  where
    dec1 _ 1 = Nothing
    dec1 s n = Just (s, n-1)

-- add piece to location, throws error on nonlegality
landPiece :: Pos -> Side -> Board -> Either InvalidDecisionType Board
landPiece pos side board@(Board b bw bb) =
  case getChip board pos of
    Nothing                 -> Right (setField (Just (side, 1)))
    Just (s, n) | s == side -> Right (setField (Just (side, n+1)))
    Just (_, n) | n == 1    -> Right (incBar (opposite side) (setField (Just (side, 1))))
    _                       -> Left (MovedOntoOpponentsClosedPoint pos)
  where
    setField f = Board (take (pos) b ++ [f] ++ drop (pos+1) b) bw bb

-- Get legal moves for a single dice (handles any value 1..36)
-- get_normal_moves from backgammon.py
singleDieLegalMoves :: Board -> Die -> Side -> [Move]
singleDieLegalMoves bd d side = moves 1 where
  moves :: Pos -> [Move]
  moves 25 = []
  moves i2 =
    case getChip bd i2 of
      Nothing -> nextMoves
      -- if side same as chip, and 1 <= pos,move_pos <= 24 (in the board)
      Just (s,_) -> if (s == side && i2<=24 && ni <= 24 && i2>=1 && ni>=1)
                    then case getChip bd ni of
                      -- Check if move_pos is legal
                      Nothing -> Move i2 ni : nextMoves
                      Just (s2,n2)  | s2==side ->   Move i2 ni : nextMoves
                                    | otherwise ->  if (n2==1)
                                                    then Move i2 ni : nextMoves
                                                    else nextMoves
                    else nextMoves
      where nextMoves = moves (i2+1)
            -- find next move_pos
            ni = (i2 + d * direction side)


-- Checks if bearing off is possible
-- Only possible if
-- For Black = no chips on any triangle b/w [7..24]
-- For White = no chips on any triangle b/w [1..18]
canBearOff :: Board -> Side -> Bool
canBearOff (Board b bw bb) side
  | side==Black = (bb==0) && checkChipSide (takeLast 19 b) side
  | otherwise   = (bw==0) && checkChipSide (take 19 b) side

-- play bear off move, assumes bear off possible
bearOffMoves :: Board -> Die -> Side -> [Move]
bearOffMoves bd dieRoll side =
  directMoves ++ homeMoves ++ bigBearOff where
    -- direct bearoffs (if chip at 5 away from bearoff and die roll 5)
    directMoves :: [Move]
    directMoves | not (checkChipSide [(getChip bd ind)] side) = [(BearOff ind end)]
                | otherwise = [] where
                  ind = if side==White then (25-dieRoll) else dieRoll
    -- Single die moves for dieRoll, within homeboard no bearing off
    homeMoves = singleDieLegalMoves bd dieRoll side
    directHome = directMoves ++ homeMoves
    -- If nth works out, then you can bearOff from indexes < dieRoll
    bigBearOff  | length(directHome)==0 = bigBearOffFunc 1
                | otherwise = [] where
                  bigBearOffFunc 7 = []
                  bigBearOffFunc i =
                    case getChip bd ind2 of
                      Nothing -> bigBearOffFunc (i+1)
                      Just (s,_)  | s==side && i<dieRoll -> BearOff ind2 end : bigBearOffFunc (i+1)
                                  | otherwise -> bigBearOffFunc (i+1)
                      where ind2 = if side==White then (25-i) else i
    end = if side==White then 25 else 0


barMoves :: Board -> Die -> Side -> [Move]
barMoves bd dieRoll side =
  case getChip bd ind of
    Nothing -> [(Enter (-1) ind)]
    Just (s,_)  | s==side -> [(Enter (-1) ind)]
                | otherwise -> []
    where ind = if side==White then (25-dieRoll) else dieRoll

-- Keep only unique moves from list of moves.
-- Moves can be reversed as well
uniqueMoves :: [[Move]] -> [[Move]]
uniqueMoves xs = uniqueMoves' Set.empty xs where
  uniqueMoves' :: Set.Set([Move]) -> [[Move]] -> [[Move]]
  uniqueMoves' _ [] = []
  uniqueMoves' s (x:xss)
   | x `Set.member` s || (reverse x) `Set.member` s = uniqueMoves' s xss
   | otherwise = x : uniqueMoves' (Set.insert x s) xss

-- Given a dice roll, board and a side, it gives legal moves
-- Moves can be run on function move directly
-- Checks bear offs, bar and normal moves as well
-- Handles double dice rolls and permutations of dice
legalMoves :: Board -> Dice -> Side -> [[Move]]
legalMoves bdM dice side
  -- if both die values same, double dice roll
  | (length dieRollsM == 4) = legalMoves' (Right bdM) dieRollsM
  -- otherwise dice + reverse dice
  | otherwise = uniqueMoves (legalMoves' (Right bdM) dieRollsM ++
                            legalMoves' (Right bdM) (reverse dieRollsM))
  where dieRollsM = dieList dice
        legalMoves' :: Either InvalidDecisionType Board -> [Die] -> [[Move]]
        legalMoves' _ [] = [[]]
        legalMoves' (Left _) _ = [[]]
        legalMoves' (Right bd@(Board _ bw bb)) dieRolls
          -- check bar moves
          | (side==White && bw/=0) || (side==Black && bb/=0) =
            if (length bMoves /= 0)
            then [m:ms |  m <- bMoves,
                          ms <- legalMoves' (move side bd m) nDRolls]
            else legalMoves' (Right bd) nDRolls
          -- check bear off moves
          -- check single die move
          | (length nMoves /= 0) =
            [m:ms | m <- nMoves,
                    ms <- legalMoves' (move side bd m) nDRolls]
          -- if no move possible with die, then go to the next die
          | otherwise = legalMoves' (Right bd) nDRolls where
              dRoll = head dieRolls
              nDRolls = tail dieRolls
              bMoves = barMoves bd dRoll side
              nMoves =  if (canBearOff bd side)
                        then (bearOffMoves bd dRoll side)
                        else (singleDieLegalMoves bd dRoll side)

-- change game state to given state
moveToState :: GameState -> Game -> Game
moveToState state game = game { gameState = state }

-- add new action to action list in game
appendAction :: GameAction -> Game -> Game
appendAction action game = game { gameActions = gameActions game ++ [action] }

-- change state and add action for a game
success :: Game -> GameState -> GameAction -> Either InvalidAction Game
success game state action =
  Right ((appendAction action . moveToState state) game)

-- stop at the first error, otherwise continue
first :: (a -> c) -> Either a b -> Either c b
first f (Left l)  = Left (f l)
first _ (Right r) = Right r

-- check if game has ended (no pieces left on board of side)
checkGameEnd :: Board -> Side -> Bool
checkGameEnd (Board b bw bb) side = (bar==0) && (checkChipSide b side) where
  bar = if side==White then bw else bb

-- handles the different actions
performAction :: GameAction -> Game -> Either InvalidAction Game
-- Initial dice throw
performAction act@(InitialThrows dw db) game@Game{gameState = PlayersToThrowInitial} =
  success game (if dw /= db then ToMove side (normDice (dw,db))
                else PlayersToThrowInitial) act where
                  side = if dw > db then White else Black
-- Move
performAction act@(PlayerAction pSide m@(Moves moves)) game@Game{gameState = ToMove side _} | pSide==side =
  do  updatedBoard <- wrapInInvalidDecision (foldM (move side) board moves)
      if (checkGameEnd updatedBoard side)
      then success (game {gameBoard = updatedBoard}) (GameFinished side) act -- Game finished
      else success (game {gameBoard = updatedBoard}) (ToThrow (opposite side)) act where -- If not finished, then throw for opposite side
            board = gameBoard game
            wrapInInvalidDecision = first (InvalidPlayerDecision game m)
-- Throw
performAction act@(PlayerAction pSide (Throw dice)) game@Game{gameState = ToThrow side} | pSide==side =
  success game (ToMove side dice) act
-- Any other action is invalid
performAction action game = Left (ActionInvalidForState (gameState game) action)

-- Max die first
normDice :: Dice -> Dice
normDice (d1, d2) = if d1 > d2 then (d1, d2) else (d2, d1)

-- White starts
initialThrowWhite :: Int -> GameAction
initialThrowWhite seed = head [ makeInitialAction (normDice (i,j)) | (i,j) <- nRolls seed 10, i/=j] where
                                makeInitialAction (a,b)= InitialThrows a b

-- Random starts
initialThrowRandom :: Int -> GameAction
initialThrowRandom seed = head [ makeInitialAction (i,j) | (i,j) <- nRolls seed 10, i/=j] where
                                  makeInitialAction (a,b)= InitialThrows a b

-- Get n dice rolls
nRolls :: Int -> Int -> [Dice]
nRolls seed n = zip (take n s1) (take n s2)
                  where s1 = R.randomRs (1,6) (R.mkStdGen seed) :: [Die]
                        s2 = R.randomRs (1,6) (R.mkStdGen (seed+1)) :: [Die]

-- wrapper for 1000 dice rolls
diceRolls :: Int -> [Dice]
diceRolls seed = nRolls seed 1000

getRandomMove :: [[Move]] -> Int -> [Move]
getRandomMove [] _ = []
getRandomMove moves seed = moves !! (head $ R.randomRs (0,((length moves)-1)) (R.mkStdGen seed) :: Int)

-- loops through and plays a game
-- game always starts with white, change initialThrowWhite to initialThrowRandom to random start
-- handles state and then loop, ends at game end only
-- seed defines randomness, reproducable results
gamePlay :: Side -> Int -> Int -> Int -> Either InvalidAction Game
gamePlay pSide depth pruningDepth seed = gamePlay' newGame 1 where
  dRolls = diceRolls seed
  gamePlay' :: Game -> Int -> Either InvalidAction Game
  -- handle initial throw
  gamePlay' game@Game{gameState = PlayersToThrowInitial} n =
    do
      nextGame <- performAction (initialThrowWhite seed) game
      gamePlay' nextGame n
  -- Move
  -- Chooses first move
  gamePlay' game@Game{gameState = ToMove side dice} n =
    do
      let board = gameBoard game
      let validMoves = legalMoves board dice side
      -- let randomMove = if (length validMoves > 0) then (head validMoves) else []
      let randomMove = getRandomMove validMoves seed
      let moveI =  if pSide == side
                  then bestMove board dice side depth pruningDepth
                  else randomMove
      nextGame <- performAction (PlayerAction side (Moves moveI)) game
      gamePlay' nextGame n
  -- Dice throw
  gamePlay' game@Game{gameState = ToThrow side} n =
    do
      nextGame <- performAction (PlayerAction side (Throw (dRolls !! n))) game
      gamePlay' nextGame (n+1)
  -- End game
  gamePlay' game@Game{gameState = GameFinished _} _ = Right (game)

-- Helper func - Takes a list of points, and returns a list of Ints
-- +1 for White at every point
-- -1 for Black at every point
pointCounter :: Point -> Int
pointCounter point = case point of
  Nothing -> 0
  Just(a,b) -> case a of
    White -> b
    Black -> (-b)

homeBoardChips :: Board -> Side -> Int
homeBoardChips bd side = sum [(checkChip i) | i <- range] where
  range = if side==White then [19..24] else [1..6]
  checkChip ind = case getChip bd ind of
    Nothing -> 0
    Just (s,n) -> if (s==side) then n else 0

eval :: Board -> Side -> Int
-- eval bd@(Board b bw bb) side = trace (show $ [distance, barWeight, homeWin, homeChips, opponentChips]) finalValue where
eval bd@(Board b bw bb) side = finalValue where
  boardValues = map pointCounter b
  whitePieces = sum $ filter (>0) boardValues
  blackPieces = abs $ sum $ filter (<0) boardValues
  distanceList = case side of
    White -> filter (>0) $ zipWith (*) [24, 23..1] $ tail boardValues
    Black -> filter (<0) $ zipWith (*) [1..24] $ tail boardValues
  distance = abs $ sum distanceList
  barWeight = case side of
    White -> bw
    Black -> bb
  homeWin = case side of
    White -> 15 - bw - whitePieces
    Black -> 15 - bb - blackPieces
  -- opponentWin = case side of
  --   White -> 15 - bb - blackPieces
  --   Black -> 15 - bw - whitePieces
  homeChips = homeBoardChips bd side
  opponentChips = (homeBoardChips bd (opposite side))
  finalValue = homeChips + 10 * homeWin - distance - 10 * barWeight - opponentChips

performMoves :: Board -> Side -> [Move] -> Either InvalidDecisionType Board
performMoves board _ [] = (Right board)
performMoves board side (m:ms) = case (move side board m) of
  (Left l) -> Left l
  (Right newBoard) -> performMoves newBoard side ms

bestMove :: Board -> Dice -> Side -> Int -> Int -> [Move]
bestMove board diceRoll side depth pruningDepth = bestMove' forwardPruningMoves (-1/0) [] where
  allLegalMoves = legalMoves board diceRoll side
  forwardPruningMoves = forwardPruning board side allLegalMoves pruningDepth
  bestMove' :: (Ord t, Fractional t) => [[Move]] -> t -> [Move] -> [Move]
  bestMove' [] _ bestMoveA = bestMoveA
  bestMove' (mv:mvs) bestScore bestMoveA = case (performMoves board side mv) of
    (Left _) -> bestMove' mvs bestScore bestMoveA
    (Right upBoard) -> bestMove' mvs newBestScore newBestMove where
      expectiRes = expectinode upBoard side (opposite side) bestScore (1/0) depth pruningDepth
      newBestScore = if (expectiRes>bestScore) then expectiRes else bestScore
      newBestMove = if (expectiRes>bestScore) then mv else bestMoveA

expectinode :: (Ord t, Fractional t) => Board -> Side -> Side -> t -> t -> Int -> Int -> t
expectinode board side _ _ _ 0 _ = fromIntegral $ eval board side
expectinode board side currSide alpha beta depth pruningDepth
  | side==currSide = sumAllDice minValue
  | otherwise = sumAllDice maxValue where
    sumAllDice func = sum [(multiplier diceRoll)*(func board side currSide diceRoll alpha beta depth pruningDepth)
                            | diceRoll <- allDiceRolls]
    multiplier (d1,d2) = if (d1==d2) then (1/36) else (1/18)

minValue :: (Fractional t, Ord t) => Board -> Side -> Side -> Dice -> t -> t -> Int -> Int -> t
minValue board side currSide diceRoll alpha beta depth pruningDepth
  | length allLegalMoves > 0 = minValue' forwardPruningMoves alpha beta (1/0)
  | otherwise = expectinode board side (opposite currSide) alpha beta (depth-1) pruningDepth where
    allLegalMoves = legalMoves board diceRoll currSide
    forwardPruningMoves = forwardPruning board currSide allLegalMoves pruningDepth
    minValue' :: (Ord t, Fractional t) => [[Move]] -> t -> t -> t -> t
    minValue' [] _ _ bestScore = bestScore
    minValue' (mv:mvs) al bt bestScore = case (performMoves board currSide mv) of
      (Left _) -> minValue' mvs al bt bestScore
      (Right newBoard) -> if newBestScore <= al
                          then newBestScore
                          else minValue' mvs al newBt newBestScore where
        expectiRes = expectinode newBoard side (opposite currSide) al bt (depth-1) pruningDepth
        newBestScore = min bestScore expectiRes
        newBt = min bt newBestScore

maxValue :: (Fractional t, Ord t) => Board -> Side -> Side -> Dice -> t -> t -> Int -> Int -> t
maxValue board side currSide diceRoll alpha beta depth pruningDepth
  | length allLegalMoves > 0 = maxValue' forwardPruningMoves alpha beta (-1/0)
  | otherwise = expectinode board side (opposite currSide) alpha beta (depth-1) pruningDepth where
    allLegalMoves = legalMoves board diceRoll currSide
    forwardPruningMoves = forwardPruning board currSide allLegalMoves pruningDepth
    maxValue' :: (Ord t, Fractional t) => [[Move]] -> t -> t -> t -> t
    maxValue' [] _ _ bestScore = bestScore
    maxValue' (mv:mvs) al bt bestScore = case (performMoves board currSide mv) of
      (Left _) -> maxValue' mvs al bt bestScore
      (Right newBoard) -> if newBestScore >= bt
                          then newBestScore
                          else maxValue' mvs newAl bt newBestScore where
        expectiRes = expectinode newBoard side (opposite currSide) al bt (depth-1) pruningDepth
        newBestScore = max bestScore expectiRes
        newAl = max al newBestScore

forwardPruning :: Board -> Side -> [[Move]] -> Int -> [[Move]]
forwardPruning board side moves k
  | length moves < k = moves
  | otherwise = [mv | (_,mv) <- (take k sortedFordwardPruningList)] where
    sortedFordwardPruningList = sortBy (\x y -> compare (fst x) (fst y)) fordwardPruningList
    fordwardPruningList = zip [-1*(fordwardPruning' mv) | mv <- moves] moves
    fordwardPruning' mv = case (performMoves board side mv) of
      (Left _) -> 0
      (Right newBoard) -> eval newBoard side

-- forwardPruningK :: Board -> Side -> [[Move]] -> [[Move]]
-- forwardPruningK board side moves = forwardPruning board side moves 2
