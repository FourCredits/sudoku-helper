module Solving where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe

import Utils
import Types

-- Cell level manipulations

isFilledIn :: Cell -> Bool
isFilledIn = isJust . number

isBlank :: Cell -> Bool
isBlank = isNothing . number

hasNote :: Int -> Cell -> Bool
hasNote n = elem n . notes

-- Working with houses

row :: Int -> [Position]
row a = [(a, b) | b <- [1 .. 9]]

col :: Int -> [Position]
col a = [(b, a) | b <- [1 .. 9]]

box :: Position -> [Position]
box (r, c) = [(a, b) | a <- [r' .. r' + 2], b <- [c' .. c' + 2]]
  where
    r' = 1 + 3 * ((r - 1) `div` 3)
    c' = 1 + 3 * ((c - 1) `div` 3)

buddies :: Position -> [Position]
buddies (r, c) = (row r `union` col c `union` box (r, c)) \\ [(r, c)]

rows, cols, boxes, houses :: [[Position]]

rows = map row [1 .. 9]

cols = map col [1 .. 9]

boxes = [box (r, c) | r <- [1, 4, 9], c <- [1, 4, 9]]

houses = rows ++ cols ++ boxes

-- Utility functions

updateNotes :: Grid -> Grid
updateNotes grid = mapArray f grid
  where
    f pos cell@Cell { notes = ns } =
      let others = mapMaybe (number . (grid !)) $ buddies pos
       in cell { notes = ns \\ others }

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
    cell' = cell {notes = delete n $ notes cell}

acceptRecommendation :: Recommendation -> Grid -> Grid
acceptRecommendation changes grid = foldr acceptChange grid changes

validChange :: Grid -> Change -> Bool
validChange grid (FillInNum pos _) = isBlank $ grid ! pos
validChange grid (RemoveNote pos n) = isBlank cell && n `elem` notes cell
  where
    cell = grid ! pos

isSolved :: Grid -> Bool
isSolved grid = all (valid . map (grid !)) houses
  where
    valid house =
      all (isJust . number) house &&
      length (nub house) == length house

intersectingHouses :: [Position] -> [[Position]]
intersectingHouses house = filter ((== 3) . length . intersect house) houses

-- Finds the union of the notes of a particular group of positions
notesUnion :: Grid -> [Position] -> [Int]
notesUnion grid = foldr (union . notes) [] . filter isBlank . map (grid !)

makeRecommendation :: Grid -> Recommendation -> Maybe Recommendation
makeRecommendation grid changes =
  case filter (validChange grid) changes of
    [] -> Nothing
    changes' -> Just changes'

-- For help with x wings, swordfishes, etc.
housesWithAOrFewerOfNoteB :: Int -> Int -> Grid -> [[Position]]
housesWithAOrFewerOfNoteB a b grid = do
  house <- rows ++ cols
  guard $ Just b `notElem` map (number . (grid !)) house
  let house' = filter (liftA2 (&&) (elem b . notes) isBlank . (grid !)) house
  guard $ a >= length house'
  return house'

isRectangle :: Position -> Position -> Position -> Position -> Bool
isRectangle (r1, c1) (r2, c2) (r3, c3) (r4, c4) =
  (r1 == r2 && r3 == r4 && c1 == c3 && c2 == c4) ||
  (c1 == c2 && c3 == c4 && r1 == r3 && r2 == r4)

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
    mkChange (pos, Cell {notes = [n]}) = [FillInNum pos n]
    isNakedSingle _ cell = isBlank cell && length (notes cell) == 1

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
    let subsetNotes = notesUnion grid subset
        notesLength = length subsetNotes
        isNakedSubset =
          notesLength == length subset &&
          inRange (2, houseLength - 1) notesLength
    guard isNakedSubset
    return $
      makeRecommendation grid $ RemoveNote <$> house \\ subset <*> subsetNotes

intersection :: Recommender
intersection grid =
  asum $ do
    house <- map (filterBlanks grid) houses
    other <- map (filterBlanks grid) $ intersectingHouses house
    let shared = house `intersect` other
        alignedNotes = ((\\) `on` notesUnion grid) shared (house \\ other)
    return $
      makeRecommendation grid $
      RemoveNote <$> (other \\ shared) <*> alignedNotes

xWing :: Recommender
xWing grid =
  asum $ do
    n <- [1 .. 9]
    let housesWith2Cells = housesWithAOrFewerOfNoteB 2 n grid
    h@[pos1@(r1, c1), pos2@(r2, c2)] <- housesWith2Cells
    [pos3@(r3, c3), pos4@(r4, c4)] <- housesWith2Cells \\ [h]
    guard $ isRectangle pos1 pos2 pos3 pos4
    let otherCells =
          foldr union [] [row r1, row r4, col c1, col c4] \\
          [pos1, pos2, pos3, pos4]
    return $ makeRecommendation grid $ RemoveNote <$> otherCells <*> [n]

{-
BUG stands for 'bi-value universal grave'. It says that any grid where the
remaining cells all have 2 notes is invalid, as that grid can have two
solutions. This isn't immediately intuitive, but if you go through it manually,
you'll see that it's true.
As such, whenever a grid has remaining cells such that all have two notes except'
one, which has three, you can fill in that cell with the note it shares with two
other cells in any given house.
-}
bug :: Recommender
bug grid =
  let blanks = filter (isBlank . snd) $ assocs grid
      triValues = filter ((== 3) . length . notes . snd) blanks
      others = map (notes . snd) $ blanks \\ triValues
   in case (triValues, all ((== 2) . length) others) of
        ([(pos, cell)], True) ->
          let buds = map (grid !) $ box pos
              Just n =
                find (\n -> 3 == length (filter (hasNote n) buds)) $ notes cell
           in Just [FillInNum pos n]
        _ -> Nothing

-- To make the overall recommender better, just add more recommenders
overallRecommender :: Recommender
overallRecommender grid = asum $ map ($ grid) solvers
  where
    solvers =
      [ nakedSingle
      , hiddenSingle
      , nakedSubset
      , intersection
      , xWing
      , bug
      ]
