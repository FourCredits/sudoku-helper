module Solving where

import Data.Array.IArray
import Data.Foldable
import Data.List
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.Set (Set)
import qualified Data.Set as S

import ArrayUtils
import Types

isFilledIn :: Cell -> Bool
isFilledIn = isJust . number

isBlank :: Cell -> Bool
isBlank = isNothing . number

row :: Int -> [Position]
row a = [(a, b) | b <- [0 .. 8]]

col :: Int -> [Position]
col a = [(b, a) | b <- [0 .. 8]]

box :: Position -> [Position]
box (r, c) = [(a, b) | a <- [r' .. r' + 2], b <- [c' .. c' + 2]]
  where
    r' = 3 * (r `div` 3)
    c' = 3 * (c `div` 3)

buddies :: Position -> [Position]
buddies (r, c) = (row r `union` col c `union` box (r, c)) \\ [(r, c)]

houses :: Grid -> [[Position]]
houses grid = rows ++ cols ++ boxes
  where
    rows = map row [0 .. 8]
    cols = map col [0 .. 8]
    boxes = [box (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]

updateNotes :: Grid -> Grid
updateNotes grid = mapArray f grid
  where
    f pos cell@Cell { notes = ns } =
      let others = mapMaybe (number . (grid !)) $ buddies pos
       in cell { notes = ns I.\\ I.fromList others }

-- Look, I don't come up with the names
nakedSingle :: Recommender
nakedSingle grid = mkChange <$> findArray isNakedSingle grid
  where
    mkChange (pos, Cell {notes = ns}) = [FillInNum pos (I.findMin ns)]
    isNakedSingle _ cell = isBlank cell && I.size (notes cell) == 1

subsets :: [a] -> [[a]]
subsets = foldr (\x xs -> xs ++ map (x :) xs) [[]]

validChange :: Grid -> Change -> Bool
validChange grid (FillInNum pos _) = isBlank $ grid ! pos
validChange grid (RemoveNote pos n) = I.member n $ notes (grid ! pos)

nakedSubset :: Recommender
nakedSubset grid =
  asum $ do
    house <- map filterBlanks $ houses grid
    let houseLength = length house
    subset <- subsets house
    let subsetNotes :: IntSet
        subsetNotes = notesOfSubset subset
        notesLength = I.size subsetNotes
        subsetInv = house \\ subset
        isNakedSubset =
          notesLength == length subset &&
          inRange (1, houseLength - 1) notesLength
    -- If there's only one element not in the subset, then that one
    -- element has a hidden single
    return $
      case (isNakedSubset, length subsetInv) of
        (True, 1) -> Just $ mkHiddenSingle subsetInv subsetNotes
        (True, _) -> Just $ mkNakedSubset subsetInv subsetNotes
        _ -> Nothing
  where
    mkHiddenSingle subsetInv subsetNotes =
      let posToFill = head subsetInv
          numToFillWith = I.findMax $ notes (grid ! posToFill) I.\\ subsetNotes
       in [FillInNum posToFill numToFillWith]
    mkNakedSubset subsetInv subsetNotes =
      filter (validChange grid) $
      RemoveNote <$> subsetInv <*> I.toList subsetNotes
    filterBlanks = filter (isBlank . (grid !))
    notesOfSubset :: [Position] -> IntSet
    notesOfSubset = I.unions . map notes . filter isBlank . map (grid !)

-- To make the overall recommender better, just add more recommenders to the list
recommend :: Recommender
recommend grid = asum $ map ($ grid) [nakedSingle, nakedSubset]

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
        cell' = cell { notes = I.delete n $ notes cell }

isSolved :: Grid -> Bool
isSolved grid = all (valid . map (grid !)) $ houses grid
  where
    valid subsection =
      all (isJust . number) subsection &&
      length (nub subsection) == length subsection

{-
Continuously apply the recommender, until it either can't generate new
recommendations, or it completes the grid. Returns (True, grid) if the grid is
completed, or (False, grid) if it isn't, and this is as far as it got.
-}
solve :: Recommender -> Grid -> (Bool, Grid)
solve recommender = go
  where
    go grid
      | isSolved grid = (True, grid)
      | otherwise =
        case recommender grid of
          Just r  -> go $ updateNotes $ acceptRecommendation r grid
          Nothing -> (False, grid)

-- io

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

