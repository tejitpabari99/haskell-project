game = newGame
Right act1 = performAction (InitialThrows 4 5) game
Game bd _ _ _ = act1
mvs = legalMoves Black bd (5,4)
map (checkMovesLegal Black bd (5,4)) (toList mvs)
