module Solving where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.Set (Set)
import qualified Data.Set as S

import Utils
import Types

-- Cell level manipulations

isFilledIn :: Cell -> Bool
isFilledIn = isJust . number

isBlank :: Cell -> Bool
isBlank = isNothing . number

hasNote :: Int -> Cell -> Bool
hasNote n = I.member n . notes

-- Working with houses

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

rows, cols, boxes, houses :: [[Position]]

rows = map row [0 .. 8]

cols = map col [0 .. 8]

boxes = [box (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]

houses = rows ++ cols ++ boxes

-- Utility functions

updateNotes :: Grid -> Grid
updateNotes grid = mapArray f grid
  where
    f pos cell@Cell { notes = ns } =
      let others = mapMaybe (number . (grid !)) $ buddies pos
       in cell { notes = ns I.\\ I.fromList others }

filterBlanks :: Grid -> [Position] -> [Position]
filterBlanks grid = filter (isBlank . (grid !))

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

validChange :: Grid -> Change -> Bool
validChange grid (FillInNum pos _) = isBlank $ grid ! pos
validChange grid (RemoveNote pos n) = I.member n $ notes (grid ! pos)

isSolved :: Grid -> Bool
isSolved grid = all (valid . map (grid !)) houses
  where
    valid house =
      all (isJust . number) house &&
      length (nub house) == length house

intersectingHouses :: [Position] -> [[Position]]
intersectingHouses house = filter ((== 3) . length . intersect house) houses

-- Finds the union of the notes of a particular group of positions
notesUnion :: Grid -> [Position] -> IntSet
notesUnion grid = I.unions . map (notes . (grid !))

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

-- Recommenders

-- Look, I don't come up with the names
nakedSingle :: Recommender
nakedSingle grid = mkChange <$> findArray isNakedSingle grid
  where
    mkChange (pos, Cell {notes = ns}) = [FillInNum pos (I.findMin ns)]
    isNakedSingle _ cell = isBlank cell && I.size (notes cell) == 1

hiddenSingle :: Recommender
hiddenSingle grid =
  asum $ do
    house <- map (filterBlanks grid) houses
    n <- [1 .. 9]
    case filter (hasNote n . (grid !)) house of
      [pos] -> return $ Just [FillInNum pos n]
      _ -> return Nothing

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

intersection :: Recommender
intersection grid =
  asum $ do
    house <- map (filterBlanks grid) houses
    other <- map (filterBlanks grid) $ intersectingHouses house
    let shared = house `intersect` other
        alignedNotes =
          I.toList $
          (I.difference `on` notesUnion grid) shared (house \\ other)
        changes =
          filter (validChange grid) $
          RemoveNote <$> (other \\ shared) <*> alignedNotes
    guard $ not $ null changes
    return $ Just changes

{-
BUG stands for 'bi-value universal grave'. It says that any grid where the
remaining cells all have 2 notes is invalid, as that grid can have two
solutions. This isn't immediately intuitive, but if you go through it manually,
you'll see that it's true.
As such, whenever a grid has remaining cells such that all have two notes except
one, which has three, you can fill in that cell with the note it shares with two
other cells in any given house.
-}
bug :: Recommender
bug grid =
  let blanks = filter (isBlank . snd) $ assocs grid
      triValues = filter ((== 3) . I.size . notes . snd) blanks
      others = map (notes . snd) $ blanks \\ triValues
   in case (triValues, all ((== 2) . I.size) others) of
        ([(pos, cell)], True) ->
          let buds = map (grid !) $ box pos
              Just n =
                find (\n -> 3 == length (filter (hasNote n) buds)) $
                I.toList $ notes cell
           in Just [FillInNum pos n]
        _ -> Nothing

-- To make the overall recommender better, just add more recommenders
overallRecommender :: Recommender
overallRecommender grid = asum $ map ($ grid) solvers
  where
    solvers = [nakedSingle, hiddenSingle, nakedSubset, intersection, bug]
