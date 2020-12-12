-- module Backgammon.Gameplay () where
-- import Backgammon.Model
import Model

die1 :: Int
die1 = 2

die2 :: Int
die2 = 1

fromRight :: Show a => Either a b -> b
fromRight (Right v) = v
fromRight (Left v) = error ("expected Right but got Left " ++ show v)

start = fromRight (performAction (InitialThrows die1 die2) newGame)
act1 =  PlayerAction White (Moves [Move 24 23, Move 23 21])
s1 = fromRight(performAction act1 start)
act2 = PlayerAction Black (Throw (4, 2))
s2 = fromRight(performAction act2 s1)
