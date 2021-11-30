module TestGrids where

import Data.Array
import qualified Data.IntSet as I

import Types
import Solving

numsToGrid :: [Int] -> Grid
numsToGrid = updateNotes . listArray ((0, 0), (8, 8)) . map makeCell
  where
    makeCell :: Int -> Cell
    makeCell n =
      Cell
        (if inRange (1, 9) n
           then Just n
           else Nothing)
        (I.fromDistinctAscList [1 .. 9])

testGrid, solvedGrid :: Grid

testGrid =
  numsToGrid
    [ 6, 9, 0, 1, 7, 0, 0, 0, 0
    , 3, 5, 0, 0, 4, 0, 1, 0, 0
    , 8, 0, 2, 3, 6, 0, 7, 0, 4
    , 0, 0, 0, 0, 0, 0, 9, 0, 8
    , 9, 4, 0, 0, 0, 0, 0, 0, 5
    , 0, 0, 0, 5, 9, 0, 6, 3, 0
    , 2, 6, 0, 0, 5, 0, 0, 8, 7
    , 0, 8, 1, 0, 0, 6, 4, 9, 0
    , 0, 7, 3, 0, 8, 2, 0, 1, 0
    ]

testGrid2 :: Grid
testGrid2 =
  numsToGrid
    [ 5, 7, 0, 3, 0, 2, 0, 6, 4
    , 3, 4, 1, 6, 0, 9, 2, 7, 8
    , 9, 2, 6, 7, 0, 8, 3, 5, 1
    , 1, 0, 3, 8, 2, 6, 4, 0, 7
    , 4, 0, 2, 0, 3, 0, 8, 0, 6
    , 0, 0, 7, 1, 0, 4, 5, 0, 0
    , 0, 1, 5, 0, 0, 0, 6, 4, 0
    , 0, 0, 0, 0, 0, 0, 0, 0, 0
    , 6, 0, 4, 9, 0, 5, 1, 0, 3
    ]

testGrid3 :: Grid
testGrid3 =
  numsToGrid
    [ 2, 0, 0, 0, 0, 0, 0, 0, 0
    , 4, 7, 0, 3, 0, 8, 0, 2, 0
    , 9, 0, 1, 0, 0, 0, 8, 0, 4
    , 6, 1, 4, 8, 7, 5, 9, 3, 2
    , 3, 9, 7, 1, 2, 6, 4, 5, 8
    , 8, 5, 2, 4, 9, 3, 7, 1, 6
    , 1, 0, 3, 0, 0, 0, 5, 0, 7
    , 5, 2, 0, 7, 0, 1, 0, 4, 0
    , 7, 0, 0, 0, 0, 0, 0, 0, 0
    ]

testGrid4 :: Grid
testGrid4 =
  numsToGrid
  [ 0, 0, 3, 0, 0, 0, 0, 0, 0
  , 4, 0, 9, 0, 3, 0, 6, 1, 0
  , 5, 8, 0, 1, 0, 0, 0, 3, 0
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 8, 0, 0, 3, 0, 0, 0, 5, 0
  , 0, 9, 0, 6, 2, 0, 4, 0, 0
  , 0, 0, 7, 0, 0, 0, 0, 4, 8
  , 0, 0, 0, 7, 0, 0, 9, 0, 0
  , 9, 0, 0, 0, 1, 0, 7, 2, 0
  ]

solvedGrid =
  numsToGrid
    [ 6, 9, 4, 1, 7, 5, 8, 2, 3
    , 3, 5, 7, 2, 4, 8, 1, 6, 9
    , 8, 1, 2, 3, 6, 9, 7, 5, 4
    , 1, 3, 5, 6, 2, 7, 9, 4, 8
    , 9, 4, 6, 8, 1, 3, 2, 7, 5
    , 7, 2, 8, 5, 9, 4, 6, 3, 1
    , 2, 6, 9, 4, 5, 1, 3, 8, 7
    , 5, 8, 1, 7, 3, 6, 4, 9, 2
    , 4, 7, 3, 9, 8, 2, 5, 1, 6
    ]
