import qualified Data.Point         as P
import qualified Data.PriorityQueue as PQ
import qualified Data.WalkableMap   as WM
import           Test.Hspec

createItem :: (Int, Int) -> Int -> String -> PQ.Item String
createItem p s e = PQ.Item {PQ.location = p, PQ.score = s, PQ.extra = e}

simpleQueue =
  flip PQ.addItem (createItem (8, 2) 5 "Something else") $
  flip PQ.addItem (createItem (1, 2) 8 "Anything else") $
  PQ.addItem PQ.singleton (createItem (1, 1) 3 "Anything")

areaText :: String
areaText =
  "################\n\
  \#S #2 #        #\n\
  \#  ####  ####  #\n\
  \#  #  #  #     #\n\
  \#  #     #   ###\n\
  \#  #  #  # ###1#\n\
  \#     #  #     #\n\
  \################"

area :: WM.WalkableMap Char
area =
  snd
    $foldl
    (\((x, y), a) ch ->
       if ch == '\n'
         then ((0, y + 1), a)
         else ((x + 1, y), WM.update (x, y) ch a))
    ((0, 0), WM.fromList [] ' ')
    areaText

main :: IO ()
main =
  hspec $ do
    describe "updateScore" $ do
      it "existing value" $
        (PQ.location . fst . PQ.popMinimum . PQ.updateScore simpleQueue)
          PQ.Item
            {PQ.location = (1, 2), PQ.score = 2, PQ.extra = "Anything else"} `shouldBe`
        (1, 2)
      it "non-existing value" $
        (PQ.location . fst . PQ.popMinimum . PQ.updateScore simpleQueue)
          PQ.Item
            {PQ.location = (7, 2), PQ.score = 1, PQ.extra = "Anything else"} `shouldBe`
        (1, 1)
    describe "point" $ do
      it "add" $ (2, 3) P.<+> (1, 2) `shouldBe` (3, 5)
      it "add" $ (2, 3) P.<+> (-1, -2) `shouldBe` (1, 1)
    describe "draw" $
      it "just works?" $ WM.draw area id `shouldBe` lines areaText
    describe "valueAt" $ do
      it "start" $ WM.valueAt area (1, 1) `shouldBe` 'S'
      it "goal 1" $ WM.valueAt area (14, 5) `shouldBe` '1'
      it "goal 2" $ WM.valueAt area (4, 1) `shouldBe` '2'
      it "out of map" $ WM.valueAt area (200, 200) `shouldBe` ' '
    describe "pathTo" $ do
      it "route found" $
        WM.pathTo
          PQ.Item {PQ.location = (1, 1), PQ.score = 0, PQ.extra = []}
          (14, 5)
          area {WM.obstacles = ['#']} `shouldBe`
        Just
          (PQ.Item
             { PQ.location = (14, 5)
             , PQ.score = 0
             , PQ.extra =
                 [ (14, 5)
                 , (14, 6)
                 , (13, 6)
                 , (12, 6)
                 , (11, 6)
                 , (10, 6)
                 , (10, 5)
                 , (10, 4)
                 , (11, 4)
                 , (12, 4)
                 , (12, 3)
                 , (13, 3)
                 , (13, 2)
                 , (13, 1)
                 , (12, 1)
                 , (11, 1)
                 , (10, 1)
                 , (9, 1)
                 , (8, 1)
                 , (8, 2)
                 , (8, 3)
                 , (8, 4)
                 , (7, 4)
                 , (6, 4)
                 , (5, 4)
                 , (5, 5)
                 , (4, 5)
                 , (4, 6)
                 , (3, 6)
                 , (2, 6)
                 , (2, 5)
                 , (1, 5)
                 , (1, 4)
                 , (1, 3)
                 , (1, 2)
                 ]
             })
      it "route not found" $
        WM.pathTo
          PQ.Item {PQ.location = (1, 1), PQ.score = 0, PQ.extra = []}
          (4, 1)
          area {WM.obstacles = ['#']} `shouldBe`
        Nothing
