import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sum" $ do
    it "can sum integers" $ do
      1 + 2 `shouldBe` (3 :: Int)
