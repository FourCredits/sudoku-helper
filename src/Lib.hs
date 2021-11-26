module Lib where

import Control.Monad
import Data.Array.IArray
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid

import Types

isFilledIn :: Cell -> Bool
isFilledIn c = isJust (number c)

row :: Int -> [Position]
row a = [(a, b) | b <- [0 .. 8]]

col :: Int -> [Position]
col a = [(b, a) | b <- [0 .. 8]]

square :: Position -> [Position]
square (r, c) = [(a, b) | a <- [r' .. r' + 2], b <- [c' .. c' + 2]]
  where
    r' = 3 * (r `div` 3)
    c' = 3 * (c `div` 3)

neighbours :: Position -> [Position]
neighbours (r, c) = (row r `union` col c `union` square (r, c)) \\ [(r, c)]

mapGrid :: (Position -> Cell -> Cell) -> Grid -> Grid
mapGrid f grid = listArray (bounds grid) $ map (uncurry f) $ assocs grid

filterGrid :: (Position -> Cell -> Bool) -> Grid -> [(Position, Cell)]
filterGrid pred grid = filter (uncurry pred) $ assocs grid 

findGrid :: (Position -> Cell -> Bool) -> Grid -> Maybe (Position, Cell)
findGrid p = listToMaybe . filterGrid p

populateNotes :: Grid -> Grid
populateNotes = amap (\cell -> cell {notes = [1 .. 9]})

updateNotes :: Grid -> Grid
updateNotes grid = mapGrid f grid
  where
    f pos cell@Cell { notes = ns } =
      let others = mapMaybe (number . (grid !)) $ neighbours pos
       in cell { notes = ns \\ others }

setupNotes :: Grid -> Grid
setupNotes = updateNotes . populateNotes

-- Look, I don't come up with the names
nakedSingle :: Recommender
nakedSingle grid = mkChange <$> findGrid isNakedSingle grid
  where
    mkChange (pos, Cell {notes = [n]}) = [FillInNum pos n]
    mkChange _ = error "This should never happen"
    isNakedSingle _ (Cell Nothing [_]) = True
    isNakedSingle _ _ = False

-- To make the overall recommender better, just add more recommenders to the list
recommend :: Recommender
recommend grid = asum $ map ($ grid) [nakedSingle]

acceptRecommendation :: Recommendation -> Grid -> Grid
acceptRecommendation changes grid = foldr f grid changes
  where
    f :: Change -> Grid -> Grid
    f (FillInNum pos n) grid = grid // [(pos, cell')]
      where
        cell = grid ! pos
        cell' = cell { number = Just n }
    f (RemoveNote pos n) grid = grid // [(pos, cell')]
      where
        cell = grid ! pos
        cell' = cell { notes = notes cell \\ [n] }

-- io

numsToGrid :: [Int] -> Grid
numsToGrid ns = setupNotes $ listArray ((0, 0), (8, 8)) $ map makeCell ns
  where
    makeCell :: Int -> Cell
    makeCell n = Cell number []
      where
        number = if n `elem` [1 .. 9]
                    then Just n
                    else Nothing

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
