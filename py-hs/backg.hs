import Debug.Trace(trace)
import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified System.Random as R (mkStdGen, randomRs)

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
  deriving (Eq, Show)


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
endBoard = Board [ Just (White, 3), Just (White,2),  Nothing, Nothing, Nothing, Nothing, Just (Black, 5), Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Just (White, 5),
                   Just (Black, 5), Nothing, Nothing, Nothing, Just (White, 3), Nothing, Just (White, 5), Nothing, Nothing, Nothing, Nothing, Just (Black, 2), Nothing
                  ] 0 0

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
  Just (s,chips)  | s==side -> False
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
move side board m@(Move from to) = handleMoves board side from to
move side board m@(BearOff from to) = handleBearOffMove (handleMoves board side from to) side
move side board m@(Enter from to) = takePiece from board side >>= landPiece to side

-- Handle regular moves
handleMoves :: Board -> Side -> Pos -> Pos -> Either InvalidDecisionType Board
handleMoves board side from to =
  case getChip board from of
    Just (s,n)  -> takePiece from board side >>= landPiece to side
    Nothing     -> Left (NoPieces from)

-- Handle Bear off moves
handleBearOffMove :: Either InvalidDecisionType Board -> Side -> Either InvalidDecisionType Board
handleBearOffMove board side =
  case board of
    Right (boar@(Board bd bw bb)) | side==Black -> Right (Board ([Nothing] ++ (tail bd)) bw bb)
                                  | side==White -> Right (Board ((take 25 bd) ++ [Nothing]) bw bb)
    Left err                      -> Left err

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
    Just (s, n) | n == 1    -> Right (incBar (opposite side) (setField (Just (side, 1))))
    _                       -> Left (MovedOntoOpponentsClosedPoint pos)
  where
    setField f = Board (take (pos) b ++ [f] ++ drop (pos+1) b) bw bb
    updatedCount =
      case getChip board pos of
        Just (_, n) -> n+1
        Nothing     -> 1

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
      Just (s,n) -> if (s == side && i2<=24 && ni <= 24 && i2>=1 && ni>=1)
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
canBearOff bd@(Board b bw bb) side
  | side==Black = (bb==0) && checkChipSide (takeLast 19 b) side
  | otherwise   = (bw==0) && checkChipSide (take 19 b) side

-- play bear off move, assumes bear off possible
bearOffMoves :: Board -> Die -> Side -> [Move]
bearOffMoves bd@(Board b bw bb) dieRoll side =
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
                      Just (s,n)  | s==side && i<dieRoll -> BearOff ind2 end : bigBearOffFunc (i+1)
                                  | otherwise -> bigBearOffFunc (i+1)
                      where ind2 = if side==White then (25-i) else i
    end = if side==White then 25 else 0


barMoves :: Board -> Die -> Side -> [Move]
barMoves bd@(Board b bw bb) dieRoll side =
  case getChip bd ind of
    Nothing -> [(Enter (-1) ind)]
    Just (s,n)  | s==side -> [(Enter (-1) ind)]
                | otherwise -> []
    where ind = if side==White then (25-dieRoll) else dieRoll

-- Keep only unique moves from list of moves.
-- Moves can be reversed as well
uniqueMoves :: [[Move]] -> [[Move]]
uniqueMoves xs = uniqueMoves' Set.empty xs where
  uniqueMoves' :: Set.Set([Move]) -> [[Move]] -> [[Move]]
  uniqueMoves' _ [] = []
  uniqueMoves' s (x:xs)
   | x `Set.member` s || (reverse x) `Set.member` s = uniqueMoves' s xs
   | otherwise = x : uniqueMoves' (Set.insert x s) xs

-- Given a dice roll, board and a side, it gives legal moves
-- Moves can be run on function move directly
-- Checks bear offs, bar and normal moves as well
-- Handles double dice rolls and permutations of dice
legalMoves :: Board -> Dice -> Side -> [[Move]]
legalMoves bd dice side
  -- if both die values same, double dice roll
  | (length dieRolls == 4) = legalMoves' (Right bd) dieRolls
  -- otherwise dice + reverse dice
  | otherwise = uniqueMoves (legalMoves' (Right bd) dieRolls ++
                            legalMoves' (Right bd) (reverse dieRolls))
  where dieRolls = dieList dice
        legalMoves' :: Either InvalidDecisionType Board -> [Die] -> [[Move]]
        legalMoves' _ [] = [[]]
        legalMoves' (Left _) _ = [[]]
        legalMoves' (Right bd@(Board b bw bb)) dieRolls
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
checkGameEnd board@(Board b bw bb) side = (bar==0) && (checkChipSide b side) where
  bar = if side==White then bw else bb

-- handles the different actions
performAction :: GameAction -> Game -> Either InvalidAction Game
-- Initial dice throw
performAction act@(InitialThrows dw db) game@Game{gameState = PlayersToThrowInitial} =
  success game (if dw /= db then ToMove side (normDice (dw,db))
                else PlayersToThrowInitial) act where
                  side = if dw > db then White else Black
-- Move
performAction act@(PlayerAction pSide m@(Moves moves)) game@Game{gameState = ToMove side dice} | pSide==side =
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

-- loops through and plays a game
-- game always starts with white, change initialThrowWhite to initialThrowRandom to random start
-- handles state and then loop, ends at game end only
-- seed defines randomness, reproducable results
-- TODO: Random shuffle moves (idk how to do in haskell)
gamePlayRandom :: Int -> Either InvalidAction Game
gamePlayRandom seed = gamePlay' newGame 1 where
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
      let move = if (length validMoves > 0) then (head validMoves) else []
      nextGame <- performAction (PlayerAction side (Moves move)) game
      gamePlay' nextGame n
  -- Dice throw
  gamePlay' game@Game{gameState = ToThrow side} n =
    do
      nextGame <- performAction (PlayerAction side (Throw (dRolls !! n))) game
      gamePlay' nextGame (n+1)
  -- End game
  gamePlay' game@Game{gameState = GameFinished side} n = Right (game)

-- Player we play with
player :: Side
player = White


-- Helper func - Takes a list of points, and returns a list of Ints
-- +1 for White at every point
-- -1 for Black at every point
pointCounter point = case point of
  Nothing -> 0
  Just(a,b) -> case a of
    White -> b
    Black -> (-b)

-- TODO: Eval Func
eval :: Board -> Side -> Int
eval (Board board barWhite barBlack) side = finalValue where
  boardValues = map pointCounter board
  len = length $ filter (>0) boardValues
  distanceList = case side of
    White -> filter (>0) $ zipWith (*) [24, 23..1] $ tail boardValues
    Black -> filter (<0) $ zipWith (*) [1..24] $ tail boardValues
  distance = abs $ sum distanceList
  homeCheckers = case side of
    White -> last boardValues
    Black -> abs $ head boardValues
  opponentCheckers = case side of
    White -> abs $ head boardValues
    Black -> last boardValues
  barWeight = case side of
    White -> barWhite
    Black -> barBlack
  finalValue = 10 * homeCheckers - distance - 10 * barWeight - 10* opponentCheckers

-- TODO: Expectiminimax

-- TODO: Change gamePlayRandom to take in expecti move for a player. 
