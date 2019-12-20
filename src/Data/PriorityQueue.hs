module Data.PriorityQueue where

import Data.Point

data Item a =
  Item
    { location :: Point
    , score :: Int
    , extra :: a
    }
  deriving (Show, Eq)

type Queue a = [Item a]

singleton :: Queue a
singleton = []

sorted :: Queue a -> Queue a
sorted [] = []
sorted (x:xs) = before ++ [x] ++ after
  where
    before = sorted $ filter ((>) (score x) . score) xs
    after = sorted $ filter ((<=) (score x) . score) xs

popMinimum :: Queue a -> (Item a, Queue a)
popMinimum q = (x, xs)
  where
    (x:xs) = sorted q

updateScore :: Queue a -> Item a -> Queue a
updateScore [] _ = []
updateScore (x:xs) item
  | location x == location item =
    if score item < score x
      then item : xs
      else x : xs
  | otherwise = x : updateScore xs item

addItem :: Queue a -> Item a -> Queue a
addItem queue item
  | item `exists` queue = queue
  | otherwise = item : queue

exists :: Item a -> Queue a -> Bool
exists _ [] = False
exists item (x:xs)
  | location item == location x = True
  | otherwise = item `exists` xs
