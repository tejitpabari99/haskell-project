module Backgammon.Model
( Side (White, Black)
, Game
, Board (..) -- TODO: smart constructor and accessors
, DoublingCube
, GameAction (..)
, GameState (..)
, PlayerDecision (..)
, Move (..)
, InvalidAction (..)
, InvalidDecisionType (..)
, newGame
, gameBoard
, gameDoublingCube
, gameState
, doublingCubeOwner
, doublingCubeValue
, performAction
, performActions
, pipCount
, legalMoves
)
where

import Control.Applicative ((<$>))
import Control.Monad (foldM, foldM_, forM_)
import Data.List (elemIndex, foldl', permutations)
import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set (elems, empty, fromList)

type Pos = Int

type Die = Int

type Dice = (Die, Die)

type DoublingCubeValue = Int

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Pos Pos
          | Enter Pos
          | BearOff Pos
  deriving (Eq, Show, Ord)

data Game = Game { gameBoard :: Board
                 , _gameActions :: [GameAction]
                 , gameDoublingCube :: DoublingCube
                 , gameState :: GameState
                 }
  deriving (Eq, Show)

data Board
  -- | fields, bar white, bar black
  = Board [Maybe (Side, Int)] Int Int
  deriving (Eq, Show)

data DoublingCube = DoublingCube { doublingCubeOwner :: Maybe Side
                                 , doublingCubeValue :: DoublingCubeValue
                                 }
  deriving (Eq, Show)

data Result = Normal | Gammon | Backgammon
  deriving (Eq, Show)

data GameAction = PlayerAction Side PlayerDecision
                | InitialThrows Die Die
  deriving (Eq, Show)

data PlayerDecision = Moves [Move]
                    | Throw Dice
                    | Double
                    | AcceptDouble
                    | RejectDouble
                    | Resign Result
                    | AcceptResign
                    | RejectResign
  deriving (Eq, Show)

data GameState = PlayersToThrowInitial
               | ToMove Side Dice
               | ToDouble Side
               | ToRespondToDouble Side
               | ToThrow Side
               -- | winner, points scored
               | GameFinished Side Int
  deriving (Eq, Show)

data InvalidDecisionType = MustEnterBeforeMoving
                         | MoreMovesPossible
                         | NoPieces Pos
                         | MovedOpponentsPieces Move
                         | MovedOntoOpponentsClosedPoint Pos
                         -- | incorrect move, available numbers of pips to move by
                         | NoSuchNumberThrown Move [Die]
                         | TooManyMoves
  deriving (Eq, Show)

data InvalidAction = ActionInvalidForState GameState GameAction
                   | InvalidPlayerDecision Game PlayerDecision InvalidDecisionType
  deriving (Eq, Show)

newGame :: Game
newGame = Game initialBoard [] initialDoublingCube PlayersToThrowInitial

initialBoard :: Board
initialBoard = Board [ Just (Black, 2), Nothing, Nothing, Nothing, Nothing, Just (White, 5), Nothing, Just (White, 3), Nothing, Nothing, Nothing, Just (Black, 5)
                     , Just (White, 5), Nothing, Nothing, Nothing, Just (Black, 3), Nothing, Just (Black, 5), Nothing, Nothing, Nothing, Nothing, Just (White, 2)
                     ]
                     0
                     0

initialDoublingCube :: DoublingCube
initialDoublingCube = DoublingCube Nothing 1

normDice :: Dice -> Dice
normDice (d1, d2) = if d1 > d2 then (d1, d2) else (d2, d1)

opposite :: Side -> Side
opposite White = Black
opposite Black = White

distance :: Side -> Move -> Int
distance side (Move from to) = direction side * (to - from)
distance side (Enter to)     = direction side * (to - barIndex side)
distance side (BearOff from) = direction side * (bearOffIndex side - from)

move :: Side                                      -- ^ the side making the move
     -> Board                                     -- ^ the board
     -> Move                                      -- ^ the move to make
     -> Either InvalidDecisionType Board          -- ^ error or board
move side board m@(Move from to) =
  case pieces board from of
    Just (s, _) -> takePiece from board >>= landPiece to side
    Nothing     -> Left (NoPieces from)

takePiece :: Pos -> Board -> Either InvalidDecisionType Board
takePiece pos board@(Board b bw bb) =
  case pieces board pos of
    Just (s, n) -> Right (Board (take (pos-1) b ++ [dec1 s n] ++ drop pos b) bw bb)
    Nothing     -> Left (NoPieces pos)
  where
    dec1 _ 1 = Nothing
    dec1 s n = Just (s, n-1)

landPiece :: Pos -> Side -> Board -> Either InvalidDecisionType Board
landPiece pos side board@(Board b bw bb) =
  case pieces board pos of
    Nothing                 -> Right (setField (Just (side, 1)))
    Just (s, n) | s == side -> Right (setField (Just (side, n+1)))
    Just (s, n) | n == 1    -> Right (incBar (opposite side) (setField (Just (side, 1))))
    _                       -> Left (MovedOntoOpponentsClosedPoint pos)
  where
    setField f = Board (take (pos-1) b ++ [f] ++ drop pos b) bw bb
    updatedCount =
      case pieces board pos of
        Just (_, n) -> n+1
        Nothing     -> 1

incBar :: Side -> Board -> Board
incBar White (Board b bw bb) = Board b (bw+1) bb
incBar Black (Board b bw bb) = Board b bw     (bb+1)

pieces :: Board -> Pos -> Maybe (Side, Int)
pieces (Board board _ _) pos = board !! (pos-1)

barPieces :: Side -> Board -> Int
barPieces White (Board _ bw _ ) = bw
barPieces Black (Board _ _  bb) = bb

pointOwner :: Board -> Pos -> Maybe Side
pointOwner board pos =
  case pieces board pos of
    Just (s, n) | n > 1 -> Just s
    _                   -> Nothing

performAction :: GameAction -> Game -> Either InvalidAction Game
performAction a@(InitialThrows white black)          game@Game{ gameState = PlayersToThrowInitial } =
  success game (if white /= black then ToMove side (normDice (white, black))
                else                   PlayersToThrowInitial) a
  where
    side = if white > black then White else Black
performAction a@(PlayerAction aSide d@(Moves moves)) game@Game{ gameState = ToMove side dice }       | aSide == side = do
  _ <- wrapInInvalidDecision (checkMovesLegal side board dice moves)
  updatedBoard <- wrapInInvalidDecision (foldM (move side) board moves)
  success (game { gameBoard = updatedBoard }) (nextState (opposite side)) a
  where
    board = gameBoard game
    wrapInInvalidDecision = first (InvalidPlayerDecision game d)
    nextState = if not (ownsCube side) then ToDouble else ToThrow
    ownsCube side = (doublingCubeOwner . gameDoublingCube) game == Just side
performAction a@(PlayerAction aSide (Throw dice))    game@Game{ gameState = ToDouble side }          | aSide == side =
  success game (ToMove side (normDice dice)) a
performAction a@(PlayerAction aSide Double)          game@Game{ gameState = ToDouble side }          | aSide == side =
  success game (ToRespondToDouble (opposite side)) a
performAction a@(PlayerAction aSide AcceptDouble)    game@Game{ gameState = ToRespondToDouble side } | aSide == side =
  success (game { gameDoublingCube = acceptDouble side (gameDoublingCube game) }) (ToThrow (opposite side)) a
performAction a@(PlayerAction aSide RejectDouble)    game@Game{ gameState = ToRespondToDouble side } | aSide == side =
  success game (GameFinished (opposite side) ((doublingCubeValue . gameDoublingCube) game)) a
performAction a@(PlayerAction aSide (Throw dice))    game@Game{ gameState = ToThrow side }           | aSide == side =
  success game (ToMove side (normDice dice)) a
performAction action                                 game =
  Left (ActionInvalidForState (gameState game) action)

performActions :: [GameAction] -> Game -> Either InvalidAction Game
performActions actions game = foldl' (\eg a -> eg >>= performAction a) (Right game) actions -- TODO: use foldM?

success :: Game -> GameState -> GameAction -> Either InvalidAction Game
success game state action =
  Right ((appendAction action . moveToState state) game)

dieList :: Dice -> [Die]
dieList (d1, d2) =
  if d1 == d2 then [d1, d1, d1, d1]
              else [d1, d2]

checkMovesLegal :: Side -> Board -> Dice -> [Move] -> Either InvalidDecisionType ()
checkMovesLegal side board dice moves = do
  checkUsesAllPossibleMoves side board dice moves
  checkMovesByCorrectNumberOfPips side dice moves
  checkEntersBeforeMoving side board moves
  checkMovesOntoAllowedPoints side board moves
  checkMovesOwnPieces side board moves

checkUsesAllPossibleMoves :: Side -> Board -> Dice -> [Move] -> Either InvalidDecisionType ()
checkUsesAllPossibleMoves side board dice moves =
  if length moves < maxNoOfMovesAvailable then Left MoreMovesPossible -- TODO: include all possible moves
                                          else Right ()
  where
    maxNoOfMovesAvailable = case Set.elems (legalMoves side board dice) of
      []     -> 0
      (ms:_) -> length ms

checkMovesByCorrectNumberOfPips :: Side -> Dice -> [Move] -> Either InvalidDecisionType ()
checkMovesByCorrectNumberOfPips side dice =
  foldM_ checkAndRemoveDie (dieList dice)
  where
    checkAndRemoveDie [] _ = Left TooManyMoves
    checkAndRemoveDie ds m =
      case elemIndex (distance side m) ds of
        Just i  -> Right (take i ds ++ drop (i+1) ds)
        Nothing -> Left (NoSuchNumberThrown m ds)

checkEntersBeforeMoving :: Side -> Board -> [Move] -> Either InvalidDecisionType ()
checkEntersBeforeMoving side =
  foldM_ checkAndMove
  where
    checkAndMove board m = do
      check board m
      move side board m
    check board (Move _ _) | barPieces side board > 0 = Left MustEnterBeforeMoving
    check board _                                     = Right ()

checkMovesOntoAllowedPoints :: Side -> Board -> [Move] -> Either InvalidDecisionType ()
checkMovesOntoAllowedPoints side board moves =
  forM_ moves check
  where
    check m =
      let pos = moveTo side m
      in
        if pointOwner board pos == Just (opposite side) then Left (MovedOntoOpponentsClosedPoint pos)
                                                        else Right ()

checkMovesOwnPieces :: Side -> Board -> [Move] -> Either InvalidDecisionType ()
checkMovesOwnPieces side =
  foldM_ checkAndMove
  where
    checkAndMove board m = do
      check board m
      move side board m
    check board m =
      let from = moveFrom side m
      in
        case pieces board from of
          Just (s, n) -> if s == side then Right () else Left (MovedOpponentsPieces m)
          Nothing     -> Left (NoPieces from)

legalMoves :: Side -> Board -> Dice -> Set [Move]
legalMoves side board dice = Set.fromList (legalMoves' (Right board) (dieList dice))
  where
    legalMoves' :: Either InvalidDecisionType Board -> [Die] -> [[Move]]
    legalMoves' _ [] = [[]]
    legalMoves' (Left _) _ = [[]]
    legalMoves' (Right b) ds =
      [m:ms | (d, ds') <- selectDie ds
      ,       m        <- singleDieLegalMoves b d
      ,       ms       <- legalMoves' (move side b m) ds'
      ]
    selectDie :: [Die] -> [(Die, [Die])]
    selectDie [] = []
    selectDie ds = [(h, t) | (h:t) <- permutations ds] -- TODO: inefficient
    singleDieLegalMoves :: Board -> Die -> [Move]
    singleDieLegalMoves b d = [m | m <- moves b d 1, isLegal b m]
    moves :: Board -> Die -> Pos -> [Move]
    moves _ _ 25 = []
    moves b d i =
      case pieces b i of
        Just (s, n) -> if (s == side && i<=25 && ni <= 25) then Move i (i + d * direction side) : nextMoves
                                    else nextMoves
        Nothing -> nextMoves
      where nextMoves = moves b d (i+1)
            ni = (i + d * direction side)
    isLegal :: Board -> Move -> Bool
    isLegal _ _ = True -- TODO

moveToState :: GameState -> Game -> Game
moveToState state game = game { gameState = state }

appendAction :: GameAction -> Game -> Game
appendAction action game = game { _gameActions = _gameActions game ++ [action] }

moveFrom :: Side -> Move -> Pos
moveFrom _ (Move from _) = from
moveFrom _ (BearOff from) = from
moveFrom s (Enter _) = barIndex s

moveTo :: Side -> Move -> Pos
moveTo _ (Move _ to) = to
moveTo s (BearOff _) = bearOffIndex s
moveTo _ (Enter to) = to

direction :: Side -> Int
direction White = -1
direction Black = 1

barIndex :: Side -> Int
barIndex White = 25
barIndex Black = 0

bearOffIndex :: Side -> Int
bearOffIndex = barIndex . opposite

pipDists :: Side -> [Int]
pipDists White = [1..24]
pipDists Black = reverse [1..24]

pipCount :: Side -> Board -> Int
pipCount side (Board poss barWhite barBlack) = bar + sum (zipWith count (pipDists side) poss)
  where
    count dist (Just (s, n)) | s == side = n * dist
    count _    _                         = 0
    bar =
      25 * case side of
        White -> barWhite
        Black -> barBlack

acceptDouble :: Side -> DoublingCube -> DoublingCube
acceptDouble side (DoublingCube _ value) = DoublingCube (Just side) (value * 2)

-- TODO: replace with Data.Bifunctor (first) once we move to base 4.8+
first :: (a -> c) -> Either a b -> Either c b
first f (Left l)  = Left (f l)
first _ (Right r) = Right r

{-
perform :: PlayerDecision -> Game -> Either InvalidDecision Game
perform = error "TODO"

resultMultiplier :: Result -> Int
resultMultiplier Normal = 1
resultMultiplier Gammon = 2
resultMultiplier Backgammon = 3
-}
