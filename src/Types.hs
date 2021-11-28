module Types where

import Data.Array.IArray
import Data.IntSet (IntSet)
import qualified Data.IntSet as S

data Cell =
  Cell
    { number :: Maybe Int
    , notes :: IntSet
    }
  deriving (Show, Eq)

type Position = (Int, Int)

type Grid = Array Position Cell

data Change
  = RemoveNote Position Int
  | FillInNum Position Int
  deriving (Show, Eq)

type Recommendation = [Change]

type Recommender = Grid -> Maybe Recommendation

