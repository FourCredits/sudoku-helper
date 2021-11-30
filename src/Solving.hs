module Solving where

import Control.Monad
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

houses :: [[Position]]
houses = rows ++ cols ++ boxes
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

filterBlanks :: Grid -> [Position] -> [Position]
filterBlanks grid = filter (isBlank . (grid !))

hiddenSingle :: Recommender
hiddenSingle grid =
  asum $ do
    house <- map (filterBlanks grid) houses
    n <- [1 .. 9]
    case filter (hasNote n . (grid !)) house of
      [pos] -> return $ Just [FillInNum pos n]
      _ -> return Nothing
  where
    hasNote n cell = I.member n $ notes cell

subsets :: [a] -> [[a]]
subsets = foldr (\x xs -> xs ++ map (x :) xs) [[]]

validChange :: Grid -> Change -> Bool
validChange grid (FillInNum pos _) = isBlank $ grid ! pos
validChange grid (RemoveNote pos n) = I.member n $ notes (grid ! pos)

nakedSubset :: Recommender
nakedSubset grid =
  asum $ do
    house <- map (filterBlanks grid) houses
    let houseLength = length house
    subset <- subsets house
    let subsetNotes = notesOfSubset subset
        notesLength = I.size subsetNotes
        isNakedSubset =
          notesLength == length subset &&
          inRange (2, houseLength - 1) notesLength
        changes = RemoveNote <$> house \\ subset <*> I.toList subsetNotes
        validChanges = filter (validChange grid) changes
    guard isNakedSubset
    -- There needs to be changes to make
    guard $ not $ null validChanges
    return $ Just validChanges
  where
    notesOfSubset = I.unions . map notes . filter isBlank . map (grid !)

-- To make the overall recommender better, just add more recommenders to the list
overallRecommender :: Recommender
overallRecommender grid =
  asum $ map ($ grid) [nakedSingle, hiddenSingle, nakedSubset]

acceptChange :: Change -> Grid -> Grid
acceptChange (FillInNum pos n) grid = grid // [(pos, cell')]
  where
    cell = grid ! pos
    cell' = cell {number = Just n}
acceptChange (RemoveNote pos n) grid = grid // [(pos, cell')]
  where
    cell = grid ! pos
    cell' = cell {notes = I.delete n $ notes cell}

acceptRecommendation :: Recommendation -> Grid -> Grid
acceptRecommendation changes grid = foldr acceptChange grid changes

isSolved :: Grid -> Bool
isSolved grid = all (valid . map (grid !)) houses
  where
    valid house =
      all (isJust . number) house &&
      length (nub house) == length house

{-
Continuously apply the recommender, until it either can't generate new
recommendations, or it completes the grid. Returns (rs, grid), where rs is the
list of changes made, and grid is as far as it got.
-}
solve :: Recommender -> Grid -> ([Recommendation], Grid)
solve recommender = go []
  where
    go rs grid
      | Just r <- recommender grid =
        go (r : rs) $ updateNotes $ acceptRecommendation r grid
      | otherwise = (reverse rs, grid)
