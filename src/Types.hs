module Types where

import Data.Array.IArray

data Cell =
  Cell
    { number :: Maybe Int
    , notes :: [Int]
    }
  deriving (Show)

type Position = (Int, Int)

type Grid = Array Position Cell

data Change
  = RemoveNote Position Int
  | FillInNum Position Int
  deriving (Show, Eq)

type Recommendation = [Change]

type Recommender = Grid -> Maybe Recommendation
