module Rendering where

import Data.Array.IArray
import Data.Foldable
import Data.Maybe
import qualified Data.IntSet as I
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Types
import Utils
import Solving

renderSolution :: Grid -> Diagram B
renderSolution grid =
  hsep 1 [renderGrid grid, renderGrid (snd $ solve overallRecommender grid)]

renderGrid :: Grid -> Diagram B
renderGrid = fold . mapArray f
  where
    f (r, c) = translate (V2 (fromIntegral c) (8 - fromIntegral r)) . renderCell

renderCell :: Cell -> Diagram B
renderCell Cell {number = Just n} =
  (show n & text & scale 0.9) <> (square 1 & fc lightgrey)
renderCell Cell {notes = ns} =
  (foldMap renderNote (I.toList ns) & scale 0.9) <> square 1

renderNote :: Int -> Diagram B
renderNote n = scale (1 / 3) $ translate (V2 x y) $ text $ show n
  where
    y
      | n `elem` [1 .. 3] = 1
      | n `elem` [4 .. 6] = 0
      | n `elem` [7 .. 9] = -1
    x
      | n `elem` [1, 4, 7] = -1
      | n `elem` [2, 5, 8] = 0
      | n `elem` [3, 6, 9] = 1
