import Control.Monad
import Data.List
import Test.HUnit

import qualified Solving
import Types

main :: IO ()
main = void $ runTestTT $ TestList tests
  where
    tests = [nakedSingle, hiddenSingle, nakedSubset, intersection, xWing, bug]

recommenderTest :: String -> Recommendation -> Recommender -> Grid -> Test
recommenderTest description expected recommender grid =
  description ~: Just expected @=? sort <$> recommender grid

nakedSingle :: Test
nakedSingle = recommenderTest "naked single testing" nsA Solving.nakedSingle ns
  where
    nsA = [FillInNum (2, 1) 5]
    ns =
      Solving.numsToGrid
        [ 2, 4, 6, 0, 7, 0, 0, 3, 8
        , 0, 0, 0, 3, 0, 6, 0, 7, 4
        , 3, 7, 0, 0, 4, 0, 6, 0, 0
        , 0, 0, 8, 0, 2, 0, 7, 0, 0
        , 1, 0, 0, 0, 0, 0, 0, 0, 6
        , 0, 0, 7, 0, 3, 0, 4, 0, 0
        , 0, 0, 4, 0, 8, 0, 0, 6, 9
        , 8, 6, 0, 4, 0, 0, 0, 0, 7
        , 9, 1, 0, 0, 6, 0, 0, 4, 2
        ]

hiddenSingle :: Test
hiddenSingle =
  "hidden single testing" ~:
  TestList
    [ recommenderTest "hidden single in box" boxA Solving.hiddenSingle box
    , recommenderTest "hidden single in row" rowA Solving.hiddenSingle row
    , recommenderTest "hidden single in col" colA Solving.hiddenSingle col
    ]
  where
    boxA = [FillInNum (7, 7) 5]
    box =
      Solving.numsToGrid
        [ 2, 4, 6, 0, 7, 0, 0, 3, 8
        , 5, 8, 0, 3, 0, 6, 2, 7, 4
        , 3, 7, 0, 0, 4, 0, 6, 0, 0
        , 4, 0, 8, 6, 2, 0, 7, 0, 0
        , 1, 0, 0, 7, 5, 4, 0, 8, 6
        , 6, 0, 7, 0, 3, 0, 4, 0, 0
        , 7, 0, 4, 0, 8, 0, 0, 6, 9
        , 8, 6, 5, 4, 0, 0, 0, 0, 7
        , 9, 1, 3, 5, 6, 7, 8, 4, 2
        ]
    rowA = [FillInNum (1, 2) 4]
    row =
      Solving.numsToGrid
        [ 2, 0, 0, 0, 7, 0, 0, 3, 8
        , 0, 0, 0, 0, 0, 6, 0, 7, 0
        , 3, 0, 0, 0, 4, 0, 6, 0, 0
        , 0, 0, 8, 0, 2, 0, 7, 0, 0
        , 1, 0, 0, 0, 0, 0, 0, 0, 6
        , 0, 0, 7, 0, 3, 0, 4, 0, 0
        , 0, 0, 4, 0, 8, 0, 0, 0, 9
        , 8, 6, 0, 4, 0, 0, 0, 0, 0
        , 9, 1, 0, 0, 6, 0, 0, 0, 2
        ]
    colA = [FillInNum (2, 1) 5]
    col =
      Solving.numsToGrid
        [ 2, 4, 6, 0, 7, 0, 0, 3, 8
        , 0, 8, 0, 3, 0, 6, 2, 7, 4
        , 3, 7, 0, 0, 4, 0, 6, 0, 0
        , 4, 0, 8, 6, 2, 0, 7, 0, 0
        , 1, 0, 0, 7, 0, 4, 0, 8, 6
        , 6, 0, 7, 0, 3, 0, 4, 0, 0
        , 7, 0, 4, 0, 8, 0, 0, 6, 9
        , 8, 6, 0, 4, 0, 0, 0, 0, 7
        , 9, 1, 3, 5, 6, 7, 8, 4, 2
        ]

nakedSubset :: Test
nakedSubset =
  "naked subset testing" ~:
  TestList
    [ recommenderTest "naked pair"    npA Solving.nakedSubset np
    , recommenderTest "naked triple"  ntA Solving.nakedSubset nt
    , recommenderTest "naked quad"    nqA Solving.nakedSubset nq
    , recommenderTest "hidden pair"   hpA Solving.nakedSubset hp
    , recommenderTest "hidden triple" htA Solving.nakedSubset ht
    , recommenderTest "hidden quad"   hqA Solving.nakedSubset hq
    ]
  where
    npA =
      [ RemoveNote (1, 4) 1
      , RemoveNote (1, 5) 1
      , RemoveNote (1, 5) 6
      , RemoveNote (1, 6) 6
      ]
    np =
      Solving.numsToGrid
        [ 4, 0, 0, 0, 0, 0, 9, 3, 8
        , 0, 3, 2, 0, 9, 4, 1, 0, 0
        , 0, 9, 5, 3, 0, 0, 2, 4, 0
        , 3, 7, 0, 6, 0, 9, 0, 0, 4
        , 5, 2, 9, 0, 0, 1, 6, 7, 3
        , 6, 0, 4, 7, 0, 3, 0, 9, 0
        , 9, 5, 7, 0, 0, 8, 3, 0, 0
        , 0, 0, 3, 9, 0, 0, 4, 0, 0
        , 2, 4, 0, 0, 3, 0, 7, 0, 9
        ]
    ntA =
      [ RemoveNote (5, 1) 5
      , RemoveNote (5, 1) 9
      , RemoveNote (5, 3) 5
      , RemoveNote (5, 3) 9
      , RemoveNote (5, 7) 5
      , RemoveNote (5, 7) 8
      , RemoveNote (5, 7) 9
      , RemoveNote (5, 8) 5
      , RemoveNote (5, 8) 8
      , RemoveNote (5, 8) 9
      , RemoveNote (5, 9) 5
      , RemoveNote (5, 9) 8
      ]
    nt =
      Solving.numsToGrid
        [ 0, 7, 0, 4, 0, 8, 0, 2, 9
        , 0, 0, 2, 0, 0, 0, 0, 0, 4
        , 8, 5, 4, 0, 2, 0, 0, 0, 7
        , 0, 0, 8, 3, 7, 4, 2, 0, 0
        , 0, 2, 0, 0, 0, 0, 0, 0, 0
        , 0, 0, 3, 2, 6, 1, 7, 0, 0
        , 0, 0, 0, 0, 9, 3, 6, 1, 2
        , 2, 0, 0, 0, 0, 0, 4, 0, 3
        , 1, 3, 0, 6, 4, 2, 0, 7, 0
        ]
    nqA =
      [ RemoveNote (1, 2) 1
      , RemoveNote (1, 2) 5
      , RemoveNote (1, 3) 5
      , RemoveNote (2, 3) 5
      , RemoveNote (2, 3) 6
      , RemoveNote (2, 3) 8
      , RemoveNote (3, 3) 6
      ]
    nq =
      Solving.numsToGrid
        [ 0, 0, 0, 0, 3, 0, 0, 8, 6
        , 0, 0, 0, 0, 2, 0, 0, 4, 0
        , 0, 9, 0, 0, 7, 8, 5, 2, 0
        , 3, 7, 1, 8, 5, 6, 2, 9, 4
        , 9, 0, 0, 1, 4, 2, 3, 7, 5
        , 4, 0, 0, 3, 9, 7, 6, 1, 8
        , 2, 0, 0, 7, 0, 3, 8, 5, 9
        , 0, 3, 9, 2, 0, 5, 4, 6, 7
        , 7, 0, 0, 9, 0, 4, 1, 3, 2
        ]
    hpA =
      [ RemoveNote (1, 8) 2
      , RemoveNote (1, 8) 3
      , RemoveNote (1, 8) 5
      , RemoveNote (1, 8) 9
      , RemoveNote (1, 9) 3
      , RemoveNote (1, 9) 5
      , RemoveNote (1, 9) 9
      ]
    hp =
      Solving.numsToGrid
        [ 0, 0, 0, 0, 0, 4, 0, 0, 0
        , 9, 0, 4, 6, 0, 7, 0, 0, 0
        , 0, 7, 6, 8, 0, 4, 1, 0, 0
        , 3, 0, 9, 7, 0, 1, 0, 8, 0
        , 7, 0, 8, 0, 0, 0, 3, 0, 1
        , 0, 5, 1, 3, 0, 8, 7, 0, 2
        , 0, 0, 7, 5, 0, 2, 6, 1, 0
        , 0, 0, 5, 4, 0, 3, 2, 0, 8
        , 0, 0, 0, 0, 0, 0, 0, 0, 0
        ]
    htA =
      [ RemoveNote (1, 4) 4
      , RemoveNote (1, 4) 7
      , RemoveNote (1, 4) 8
      , RemoveNote (1, 7) 4
      , RemoveNote (1, 7) 9
      , RemoveNote (1, 9) 4
      , RemoveNote (1, 9) 7
      , RemoveNote (1, 9) 8
      , RemoveNote (1, 9) 9
      ]
    ht =
      Solving.numsToGrid
        [ 0, 0, 0, 0, 0, 1, 0, 3, 0
        , 2, 3, 1, 0, 9, 0, 0, 0, 0
        , 0, 6, 5, 0, 0, 3, 1, 0, 0
        , 6, 7, 8, 9, 2, 4, 3, 0, 0
        , 1, 0, 3, 0, 5, 0, 0, 0, 6
        , 0, 0, 0, 1, 3, 6, 7, 0, 0
        , 0, 0, 9, 3, 6, 0, 5, 7, 0
        , 0, 0, 6, 0, 1, 9, 8, 4, 3
        , 3, 0, 0, 0, 0, 0, 0, 0, 0
        ]
    hqA =
      [ RemoveNote (4, 4) 3
      , RemoveNote (4, 4) 7
      , RemoveNote (4, 4) 8
      , RemoveNote (4, 6) 3
      , RemoveNote (4, 6) 7
      , RemoveNote (4, 6) 8
      , RemoveNote (6, 4) 3
      , RemoveNote (6, 4) 7
      , RemoveNote (6, 4) 8
      , RemoveNote (6, 6) 3
      , RemoveNote (6, 6) 5
      , RemoveNote (6, 6) 7
      , RemoveNote (6, 6) 8
      ]
    hq =
      Solving.acceptRecommendation
        [ RemoveNote (1, 5) 3
        , RemoveNote (1, 5) 7
        , RemoveNote (1, 6) 3
        , RemoveNote (1, 6) 7
        , RemoveNote (3, 7) 3
        , RemoveNote (3, 7) 7
        , RemoveNote (4, 7) 3
        , RemoveNote (4, 7) 7
        , RemoveNote (6, 7) 3
        , RemoveNote (6, 7) 7
        ] $
      Solving.numsToGrid
        [ 9, 0, 1, 5, 0, 0, 0, 4, 6
        , 4, 2, 5, 0, 9, 0, 0, 8, 1
        , 8, 6, 0, 0, 1, 0, 0, 2, 0
        , 5, 0, 2, 0, 0, 0, 0, 0, 0
        , 0, 1, 9, 0, 0, 0, 4, 6, 0
        , 6, 0, 0, 0, 0, 0, 0, 0, 2
        , 1, 9, 6, 0, 4, 0, 2, 5, 3
        , 2, 0, 0, 0, 6, 0, 8, 1, 7
        , 0, 0, 0, 0, 0, 1, 6, 9, 4
        ]

intersection :: Test
intersection = recommenderTest "intersection" iA Solving.intersection i
  where
    iA = [RemoveNote (3, 3) 6]
    i =
      Solving.acceptRecommendation
        [ RemoveNote (4, 6) 5
        , RemoveNote (4, 6) 8
        , RemoveNote (4, 9) 8
        , RemoveNote (8, 4) 2
        , RemoveNote (8, 4) 8
        , RemoveNote (8, 6) 4
        , RemoveNote (8, 6) 8
        , RemoveNote (8, 6) 9
        , RemoveNote (2, 6) 4
        , RemoveNote (2, 6) 6
        ] $
      Solving.numsToGrid
        [ 0, 1, 7, 9, 0, 3, 6, 0, 0
        , 0, 0, 0, 0, 8, 0, 0, 0, 0
        , 9, 0, 0, 0, 0, 0, 5, 0, 7
        , 0, 7, 2, 0, 1, 0, 4, 3, 0
        , 0, 0, 0, 4, 0, 2, 0, 7, 0
        , 0, 6, 4, 3, 7, 0, 2, 5, 0
        , 7, 0, 1, 0, 0, 0, 0, 6, 5
        , 0, 0, 0, 0, 3, 0, 0, 0, 0
        , 0, 0, 5, 6, 0, 1, 7, 2, 0
        ]

xWing :: Test
xWing =
  "x wing tests" ~:
  TestList
    [ recommenderTest "rows"    rowsA Solving.xWing rows
    , recommenderTest "columns" colsA Solving.xWing cols
    ]
  where
  rowsA =
    [ RemoveNote (1, 4) 7
    , RemoveNote (5, 4) 7
    , RemoveNote (8, 4) 7
    , RemoveNote (8, 8) 7
    , RemoveNote (9, 4) 7
    , RemoveNote (9, 8) 7
    ]
  rows =
    Solving.acceptRecommendation
      [ RemoveNote (4,8) 3
      , RemoveNote (4,8) 7
      , RemoveNote (5,8) 3
      , RemoveNote (5,8) 7
      , RemoveNote (5,8) 9
      , RemoveNote (5,1) 3
      ] $
    Solving.numsToGrid
      [ 1, 0, 0, 0, 0, 0, 5, 6, 9
      , 4, 9, 2, 0, 5, 6, 1, 0, 8
      , 0, 5, 6, 1, 0, 9, 2, 4, 0
      , 0, 0, 9, 6, 4, 0, 8, 0, 1
      , 0, 6, 4, 0, 1, 0, 0, 0, 0
      , 2, 1, 8, 0, 3, 5, 6, 0, 4
      , 0, 4, 0, 5, 0, 0, 0, 1, 6
      , 9, 0, 5, 0, 6, 1, 4, 0, 2
      , 6, 2, 1, 0, 0, 0, 0, 0, 5
      ]
  colsA =
    [ RemoveNote (5, 3) 2
    , RemoveNote (5, 7) 2
    , RemoveNote (5, 9) 2
    , RemoveNote (9, 4) 2
    , RemoveNote (9, 9) 2
    ]
  cols =
    Solving.acceptRecommendation
      [ RemoveNote (2, 3) 3
      , RemoveNote (2, 6) 3
      , RemoveNote (1, 7) 3
      , RemoveNote (3, 7) 3
      , RemoveNote (7, 7) 6
      , RemoveNote (8, 7) 6
      ] $
    Solving.numsToGrid
      [ 0, 2, 0, 0, 0, 0, 0, 9, 4
      , 7, 6, 0, 9, 1, 0, 0, 5, 0
      , 0, 9, 0, 0, 0, 2, 0, 8, 1
      , 0, 7, 0, 0, 5, 0, 0, 1, 0
      , 0, 0, 0, 7, 0, 9, 0, 0, 0
      , 0, 8, 0, 0, 3, 1, 0, 6, 7
      , 2, 4, 0, 1, 0, 0, 0, 7, 0
      , 0, 1, 0, 0, 9, 0, 0, 4, 5
      , 9, 0, 0, 0, 0, 0, 1, 0, 0
      ]

bug :: Test
bug = recommenderTest "bug" bA Solving.bug b
  where
    bA = [FillInNum (4, 8) 2]
    b =
      Solving.acceptRecommendation
        [ RemoveNote (4, 8) 4
        , RemoveNote (7, 5) 4
        , RemoveNote (8, 7) 4
        , RemoveNote (4, 5) 4
        , RemoveNote (5, 5) 2
        , RemoveNote (4, 5) 8
        ] $
      Solving.numsToGrid
        [ 1, 7, 4, 8, 3, 2, 5, 9, 6
        , 5, 9, 3, 4, 6, 1, 2, 7, 8
        , 6, 8, 2, 9, 5, 7, 0, 0, 1
        , 0, 6, 7, 5, 0, 0, 9, 0, 0
        , 0, 1, 9, 7, 0, 3, 6, 0, 5
        , 4, 3, 5, 0, 9, 6, 8, 0, 7
        , 3, 0, 1, 6, 0, 0, 7, 5, 9
        , 9, 0, 8, 0, 7, 5, 0, 6, 0
        , 7, 5, 6, 3, 0, 9, 0, 8, 2
        ]
