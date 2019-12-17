module Data.Point where

type Point = (Int, Int)

(<+>) :: Point -> Point -> Point
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

(<->) :: Point -> Point -> Point
(x1, y1) <-> (x2, y2) = (x1 - x2, y1 - y2)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
