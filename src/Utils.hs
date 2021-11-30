module Utils where

import Data.Array.IArray
import Data.Maybe

mapArray :: (Ix i, IArray a e, IArray a e') => (i -> e -> e') -> a i e -> a i e'
mapArray f arr = listArray (bounds arr) $ map (uncurry f) $ assocs arr

filterArray :: (Ix i, IArray a e) => (i -> e -> Bool) -> a i e -> [(i, e)]
filterArray pred = filter (uncurry pred) . assocs

findArray :: (Ix i, IArray a e) => (i -> e -> Bool) -> a i e -> Maybe (i, e)
findArray p = listToMaybe . filterArray p

subsets :: [a] -> [[a]]
subsets = foldr (\x xs -> xs ++ map (x :) xs) [[]]
