module Format
( BoardParserState (..)
, BoardParseError (..)
, BoardParseErrorType (..)
, formatBoard
, parseBoard
)
where

import Model

data BoardParserState
  -- | the text to parse, position (always 1)
  = Start String Int
  -- | partially read number of black pieces on bar (as string), remaining string to parse, position
  | BlackBar String String Int
  -- | fields parsed so far, number of black pieces on bar, remaining string to parse, position
  | Contents [Maybe (Side, Int)] Int String Int
  -- | fields parsed so far, number of black pieces on bar, side whose pieces are on the current field, remaining string to parse, position
  | PieceCount [Maybe (Side, Int)] Int Side String Int
  -- | fields parsed so far, number of black pieces on bar, side whose pieces are on the current field, partially read number of pieces (as string), remaining string to parse, position
  | PieceCountCont [Maybe (Side, Int)] Int Side String String Int
  -- | parsed fields, number of black piexes on bar, partially read number of white pieces on bar (as string), remainig string to parse, position
  | WhiteBar [Maybe (Side, Int)] Int String String Int
  -- | parsed board
  | Done Board
  -- | parse error
  | Error BoardParseErrorType
  deriving (Eq, Show)

data BoardParseError = BoardParseError String BoardParseErrorType
  deriving (Eq, Show)

data BoardParseErrorType
  -- | error in parser code
  = InvalidParserStateAtEnd BoardParserState
  -- | pos, actual, expected
  | UnexpectedCharacter Int Char [Char]
  -- | pos, state at end
  | UnexpectedEndOfInput Int BoardParserState
  -- | pos, actual points
  | InsufficientPointsSpecified Int Int
  deriving (Eq, Show)

formatBoard :: Board -> String
formatBoard (Board points barWhite barBlack) =
  formatBar 'b' barBlack ++
  "|" ++ formatPoints points ++
  formatBar 'w' barWhite
  where
    formatPoints [] = ""
    formatPoints p = (concatMap formatPoint (take 6 p)) ++ "|" ++ formatPoints (drop 6 p)
    formatPoint Nothing = "."
    formatPoint (Just (side, count)) = sideChar side : show count
    formatBar side count = if count == 0 then "" else side : show count
    sideChar Black = 'b'
    sideChar White = 'w'

-- TODO: docs
parseBoard :: String -> Either BoardParseError Board
parseBoard str =
  case parse (Start str 1) of
    Done b  -> Right b
    Error t -> Left (BoardParseError str t)
    state   -> Left (BoardParseError str (InvalidParserStateAtEnd state))
  where
    parse :: BoardParserState -> BoardParserState
    parse (Start (h:t) pos) =
      case h of
        'b' -> parse (BlackBar [] t (pos+1))
        '|' -> parse (Contents [] 0 t (pos+1))
        c   -> Error (UnexpectedCharacter pos c "|")
    parse s@(BlackBar _ [] pos) = Error (UnexpectedEndOfInput pos s)
    parse (BlackBar acc (h:t) pos) =
      if isDigit h            then parse (BlackBar (acc ++ [h]) t (pos+1))
      else if isEndOfNumber h then parse (Contents [] (read acc) t (pos+1))
      else                         Error (UnexpectedCharacter pos h (digits ++ endsOfNumber))
    parse (Contents board blackBar [] pos) =
      if length board == 24 then Done (Board board 0 blackBar)
      else                       Error (InsufficientPointsSpecified pos (length board))
    parse (Contents board blackBar (h:t) pos) =
      case h of
        '|' -> parse (Contents board blackBar t (pos+1))
        '.' -> parse (Contents (board ++ [Nothing]) blackBar t (pos+1))
        'w' -> if (length board == 24) then parse (WhiteBar board blackBar [] t (pos+1))
               else                         parse (PieceCount board blackBar White t (pos+1))
        'b' -> parse (PieceCount board blackBar Black t (pos+1))
        c   -> Error (UnexpectedCharacter pos c ".wb")
    parse state@(PieceCount _ _ _ [] pos) =
      Error (UnexpectedEndOfInput pos state)
    parse (PieceCount board blackBar side (h:t) pos) =
      if isDigit h then parse (PieceCountCont board blackBar side [h] t (pos+1))
      else              Error (UnexpectedCharacter pos h digits)
    parse state@(PieceCountCont _ _ _ _ [] pos) =
      Error (UnexpectedEndOfInput pos state)
    parse (PieceCountCont board blackBar side acc (h:t) pos) =
      if isDigit h            then parse (PieceCountCont board blackBar side (acc ++ [h]) t (pos+1))
      else if isEndOfNumber h then parse (Contents (board ++ [Just (side, read acc)]) blackBar (h:t) pos)
      else                         Error (UnexpectedCharacter pos h (digits ++ endsOfNumber))
    parse s@(WhiteBar board blackBar [] [] pos) =
      Error (UnexpectedEndOfInput pos s)
    parse (WhiteBar board blackBar acc (h:t) pos) =
      if isDigit h            then parse (WhiteBar board blackBar (acc ++ [h]) t (pos+1))
      else                         Error (UnexpectedCharacter pos h digits)
    parse (WhiteBar board blacBar acc [] pos) =
      Done (Board board (read acc) blacBar)
    digits = "0123456789"
    endsOfNumber = ".|wb"
    isDigit c = any (== c) digits -- TODO: use ascii codes (more efficient)
    isEndOfNumber c = any (== c) endsOfNumber
