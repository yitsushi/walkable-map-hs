import qualified Data.PriorityQueue as PQ
import Test.Hspec

createItem :: (Int, Int) -> Int -> String -> PQ.Item String
createItem p s e = PQ.Item {PQ.location = p, PQ.score = s, PQ.extra = e}

simpleQueue =
  flip PQ.addItem (createItem (8, 2) 5 "Something else") $
  flip PQ.addItem (createItem (1, 2) 8 "Anything else") $
  PQ.addItem PQ.singleton (createItem (1, 1) 3 "Anything")

main :: IO ()
main =
  hspec $
  describe "updateScore" $ do
    it "existing value" $
      (PQ.location . fst . PQ.popMinimum . PQ.updateScore simpleQueue)
        PQ.Item {PQ.location = (1, 2), PQ.score = 2, PQ.extra = "Anything else"} `shouldBe`
      (1, 2)
    it "non-existing value" $
      (PQ.location . fst . PQ.popMinimum . PQ.updateScore simpleQueue)
        PQ.Item {PQ.location = (7, 2), PQ.score = 1, PQ.extra = "Anything else"} `shouldBe`
      (1, 1)
