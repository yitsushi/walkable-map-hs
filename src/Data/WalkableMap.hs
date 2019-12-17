module Data.WalkableMap where

import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import Data.Point
import qualified Data.PriorityQueue as PQ

data WalkableMap a =
  WalkableMap
    { content :: M.Map Point a
    , defaultValue :: a
    , obstacles :: [a]
    , _visited :: [Point]
    , _queue :: PQ.Queue [Point]
    }

singleton :: (Eq a) => a -> WalkableMap a
singleton = fromList []

fromList :: (Eq a) => [(Point, a)] -> a -> WalkableMap a
fromList list value =
  WalkableMap
    { content = M.fromList list
    , defaultValue = value
    , obstacles = []
    , _visited = []
    , _queue = []
    }

update :: (Eq a) => Point -> a -> WalkableMap a -> WalkableMap a
update pos value m = m {content = new}
  where
    new = M.insert pos value (content m)

exists :: (Eq a) => Point -> WalkableMap a -> Bool
exists pos m =
  case M.lookup pos (content m) of
    Nothing -> False
    Just _ -> True

valueAt :: (Eq a) => WalkableMap a -> Point -> a
valueAt m pos = Maybe.fromMaybe (defaultValue m) $ M.lookup pos (content m)

findValue :: (Eq a) => a -> WalkableMap a -> [Point]
findValue value = M.keys . M.filter (== value) . content

partition :: (Eq a) => Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

pathTo ::
     (Eq a)
  => PQ.Item [Point]
  -> Point
  -> WalkableMap a
  -> Maybe (PQ.Item [Point])
pathTo from to m
  | null queue = Nothing
  | PQ.location from == to = Just from
  | otherwise = pathTo next to m {_queue = queue', _visited = visited}
  where
    visited = PQ.location from : _visited m
    queue = foldl PQ.addItem (_queue m) scored
      where
        next = filter (`notElem` visited) $ neighbors (PQ.location from) m
        scored = map mapping next
        mapping x =
          PQ.Item
            { PQ.location = x
            , PQ.score = manhattan x to
            , PQ.extra = x : PQ.extra from
            }
    (next, queue') = PQ.popMinimum queue

pathToClosestValue ::
     (Eq a) => PQ.Item [Point] -> a -> WalkableMap a -> Maybe (PQ.Item [Point])
pathToClosestValue from to m
  | null queue = Nothing
  | valueAt m (PQ.location from) == to = Just from
  | otherwise =
    pathToClosestValue next to m {_queue = queue', _visited = visited}
  where
    visited = PQ.location from : _visited m
    queue = foldl PQ.addItem (_queue m) scored
      where
        next = filter (`notElem` visited) $ neighbors (PQ.location from) m
        scored = map mapping next
        mapping x =
          PQ.Item
            { PQ.location = x
            , PQ.score = 1 + PQ.score from
            , PQ.extra = x : PQ.extra from
            }
    (next, queue') = PQ.popMinimum queue

neighbors :: (Eq a) => Point -> WalkableMap a -> [Point]
neighbors (xo, yo) area =
  filter (\x -> valueAt area x `notElem` exclude) possible
  where
    exclude = obstacles area
    possible = [(xo - 1, yo), (xo + 1, yo), (xo, yo + 1), (xo, yo - 1)]

draw :: (Eq a) => WalkableMap a -> (a -> Char) -> [String]
draw area convert =
  map (map convert) $
  partition
    width
    [valueAt area (x, y) | y <- [miny .. maxy], x <- [minx .. maxx]]
  where
    list = M.toList $ content area
    minx = minimum $ map (fst . fst) list
    maxx = maximum $ map (fst . fst) list
    miny = minimum $ map (snd . fst) list
    maxy = maximum $ map (snd . fst) list
    width = maxx - minx + 1
