module Backgammon.FormatTests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Backgammon.Model (gameBoard, newGame, Board (..), Side (..))
import Backgammon.Format (formatBoard, parseBoard)


unitTests :: TestTree
unitTests = testGroup "Backgammon.Format unit tests"
  [ testCase "initial board can be parsed" $
      parseBoard "|b2....w5|.w3...b5|w5...b3.|b5....w2|" @?= Right (gameBoard newGame)

  , testCase "pieces on the bar are parsed correctly" $
      parseBoard "b1|b1....w5|.w3...b5|w5...b3.|b5.....|w2" @?= Right boardWith1WhiteAnd2BlackOnTheBar

  , testCase "initial board formatting" $
      formatBoard (gameBoard newGame) @?= "|b2....w5|.w3...b5|w5...b3.|b5....w2|"
  
  , testCase "pieces on the bar are formatted correctly" $
      formatBoard boardWith1WhiteAnd2BlackOnTheBar @?= "b1|b1....w5|.w3...b5|w5...b3.|b5.....|w2"

  -- TODO: quickcheck for format/parse roundtrip
  ]

boardWith1WhiteAnd2BlackOnTheBar =
  (Board [ Just (Black, 1), Nothing, Nothing, Nothing, Nothing, Just (White, 5), Nothing, Just (White, 3), Nothing, Nothing, Nothing, Just (Black, 5)
         , Just (White, 5), Nothing, Nothing, Nothing, Just (Black, 3), Nothing, Just (Black, 5), Nothing, Nothing, Nothing, Nothing, Nothing
         ]
         2
         1)