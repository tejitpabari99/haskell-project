import Debug.Trace(trace)
import qualified Data.Set as Set


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
  deriving (Eq, Show, Ord)
type Moves = [Move]

-- all triangles, bar white, bar black
-- length = 26, first and last are padding 0s
-- because index with 1 and last one as overflow
data Board
  = Board Points Int Int
  deriving (Eq, Show)

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


-- Start with this board
initialBoard :: Board
initialBoard = Board [ Nothing, Just (White, 2), Nothing, Nothing, Nothing, Nothing, Just (Black, 5), Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Just (White, 5),
                      Just (Black, 5), Nothing, Nothing, Nothing, Just (White, 3), Nothing, Just (White, 5), Nothing, Nothing, Nothing, Nothing, Just (Black, 2), Nothing
                     ] 0 0

-- Can try bearing off with this board
-- Bear off Black 1 dice roll,  Black 2 dice roll,  Black 1,2 dice rolls
bearOffBoard :: Board
bearOffBoard = Board [ Nothing, Nothing, Just (Black, 3), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
                      Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
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



-- Take the last n elements from xs
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs



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


-- move chip from 'from' to 'to'
move :: Board -> Side -> Move -> Board
move board side m@(Move from to) = landPiece to side (takePiece from board side)
  -- case getChip board from of
  --   Just (_, _) -> landPiece to side (takePiece from board side)
  --   Nothing     -> error ("no getChip for move " ++ show board ++ " " ++ show from)

-- take piece from the board, throws error on nonlegality
takePiece :: Pos -> Board -> Side -> Board
takePiece (-1) board@(Board b bw bb) side
  | side==White && bw>0 = Board b (bw-1) bb
  | side==Black && bb>0 = Board b bw (bb-1)
takePiece pos board@(Board b bw bb) _ =
  case getChip board pos of
    Just (s, n) -> Board (take (pos) b ++ [dec1 s n] ++ drop (pos+1) b) bw bb
    Nothing     -> error ("no getChip for takePiece " ++ show board ++ " " ++ show pos)
  where
    dec1 _ 1 = Nothing
    dec1 s n = Just (s, n-1)

-- add piece to location, throws error on nonlegality
landPiece :: Pos -> Side -> Board -> Board
landPiece pos side board@(Board b bw bb) =
  case getChip board pos of
    Nothing                 -> setField (Just (side, 1))
    Just (s, n) | s == side -> setField (Just (side, n+1))
    Just (s, n) | n == 1    -> incBar (opposite side) (setField (Just (side, 1)))
    _                       -> error ("too many opposite chips or no " ++ show side ++
                                      " chips for landPiece " ++ show board ++ " " ++ show pos)
  where
    setField f = Board (take (pos) b ++ [f] ++ drop (pos+1) b) bw bb
    updatedCount =
      case getChip board pos of
        Just (_, n) -> n+1
        Nothing     -> 1

-- increase bar value
incBar :: Side -> Board -> Board
incBar White (Board b bw bb) = Board b (bw+1) bb
incBar Black (Board b bw bb) = Board b bw     (bb+1)

-- Checks if bearing off is possible
-- Only possible if
-- For Black = no chips on any triangle b/w [7..24]
-- For White = no chips on any triangle b/w [1..18]
canBearOff :: Board -> Side -> Bool
canBearOff bd@(Board b bw bb) side
  | side==Black = (bb==0) && checkChipSide (takeLast 19 b) side
  | otherwise   = (bw==0) && checkChipSide (take 19 b) side

-- Get legal moves for a single dice (handles any value 1..36)
-- get_normal_moves from backgammon.py
singleDieLegalMoves :: Board -> Die -> Side -> Moves
singleDieLegalMoves bd d side = moves 1 where
  moves :: Pos -> Moves
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

-- play bear off move, assumes bear off possible
bearOffMoves :: Board -> Die -> Side -> Moves
bearOffMoves bd@(Board b bw bb) dieRoll side =
  directMoves ++ homeMoves ++ bigBearOff where
    -- direct bearoffs (if chip at 5 away from bearoff and die roll 5)
    directMoves :: Moves
    directMoves | not (checkChipSide [(getChip bd ind)] side) = [(Move ind end)]
                | otherwise = [] where
                  ind = if side==White then (25-dieRoll) else dieRoll
    -- Single die moves for dieRoll
    homeMoves = singleDieLegalMoves bd dieRoll side
    directHome = directMoves ++ homeMoves
    -- If nth works out, then you can bearOff from indexes < dieRoll
    bigBearOff  | length(directHome)==0 = bigBearOffFunc 1
                | otherwise = [] where
                  bigBearOffFunc 7 = []
                  bigBearOffFunc i =
                    case getChip bd ind2 of
                      Nothing -> bigBearOffFunc (i+1)
                      Just (s,n) | s==side && i<dieRoll -> Move ind2 end : bigBearOffFunc (i+1)
                      where ind2 = if side==White then (25-i) else i
    end = if side==White then 25 else 0

barMoves :: Board -> Die -> Side -> Moves
barMoves bd@(Board b bw bb) dieRoll side =
  case getChip bd ind of
    Nothing -> [(Move (-1) ind)]
    Just (s,n)  | s==side -> [(Move (-1) ind)]
                | otherwise -> []
    where ind = if side==White then (25-dieRoll) else dieRoll

uniqueMoves :: [Moves] -> [Moves]
uniqueMoves xs = uniqueMoves' Set.empty xs where
  uniqueMoves' :: Set.Set(Moves) -> [Moves] -> [Moves]
  uniqueMoves' _ [] = []
  uniqueMoves' s (x:xs)
   | x `Set.member` s || (reverse x) `Set.member` s = uniqueMoves' s xs
   | otherwise = x : uniqueMoves' (Set.insert x s) xs

-- Given a dice roll, board and a side, it gives legal moves
-- Moves can be run on function move directly
-- Checks bear offs, bar and normal moves as well
-- Handles double dice rolls and permutations of dice
legalMoves :: Board -> Dice -> Side -> [Moves]
legalMoves bd dice side
  | (length dieRolls == 4) = legalMoves' bd dieRolls side
  | otherwise = uniqueMoves (legalMoves' bd dieRolls side ++
                            legalMoves' bd (reverse dieRolls) side)
  where dieRolls = dieList dice
        legalMoves' :: Board -> [Die] -> Side -> [Moves]
        legalMoves' _ [] _ = [[]]
        legalMoves' bd@(Board b bw bb) dieRolls side
          | (side==White && bw/=0) || (side==Black && bb/=0) =
            if (length bMoves /= 0)
            then [m:ms |  m <- bMoves,
                          ms <- legalMoves' (move bd side m) nDRolls side]
            else legalMoves' bd nDRolls side
          | (length nMoves /= 0) =
            [m:ms | m <- nMoves,
                    ms <- legalMoves' (move bd side m) nDRolls side]
          | otherwise = legalMoves' bd nDRolls side where
              dRoll = head dieRolls
              nDRolls = tail dieRolls
              bMoves = barMoves bd dRoll side
              nMoves =  if (canBearOff bd side)
                        then (bearOffMoves bd dRoll side)
                        else (singleDieLegalMoves bd dRoll side)
