# WalkableMap

```haskell
import Data.Point
import qualified Data.PriorityQueue as PQ
import qualified Data.WalkableMap as WM

data Tile
  = Wall
  | Empty
  | Player
  | Goal
  | Path
  deriving (Show, Eq)

tileToChar :: Tile -> Char
tileToChar Wall = '#'
tileToChar Empty = ' '
tileToChar Player = '@'
tileToChar Goal = '?'
tileToChar Path = '.'

type Area = WM.WalkableMap Tile

input :: String
input =
  "#######\n\
  \#  ## #\n\
  \# #   #\n\
  \#   # #\n\
  \# ##  #\n\
  \#######"

parse :: String -> [(Point, Tile)]
parse = concatMap lineMap . zip [0 ..] . lines
  where
    lineMap (y, s) = (map (swap y) . zip [0 ..]) s
    swap y (x, '#') = ((x, y), Wall)
    swap y (x, ' ') = ((x, y), Empty)

main :: IO ()
main = do
  let start = (1, 1)
      goal = (4, 4)
      m =
        WM.update goal Goal $
        WM.update start Player $
        (WM.fromList (parse input) Empty) {WM.obstacles = [Wall]}
  putStrLn $ unlines $ WM.draw m tileToChar
  let path =
        WM.pathTo
          PQ.Item {PQ.location = start, PQ.score = 0, PQ.extra = [start]}
          goal
          m
  print path
  putStrLn $
    case path of
      Nothing -> "Target is not reachable."
      Just item ->
        (unlines .
         (`WM.draw` tileToChar) .
         foldl (\m l -> WM.update l Path m) m . tail . init)
          (PQ.extra item)
```
