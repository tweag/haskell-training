module BasicsSpec where

import Basics

-- hspec
import Test.Hspec

spec :: Spec
spec =
  describe "Basics" $ do
    describe "perimeter" $ do
      it "returns 4 for a square of side 1" $ do
        perimeter (Square 1) `shouldBe` 4

      it "returns 6 for a rectangle of sides 1 and 2" $ do
        perimeter (Rectangle 1 2) `shouldBe` 6
