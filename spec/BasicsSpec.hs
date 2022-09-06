module BasicsSpec where

import Basics

-- hspec
import Test.Hspec
import Test.Hspec.QuickCheck

-- QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "Basics" $ do
    describe "perimeter" $ do
      it "returns 4 for a square of side 1" $ do
        perimeter (Square 1) `shouldBe` 4

      it "returns 6 for a rectangle of sides 1 and 2" $ do
        perimeter (Rectangle 1 2) `shouldBe` 6

      prop "does not change for rectangles with swapped sides" $ do
        \s1 s2 -> perimeter (Rectangle s1 s2) `shouldBe` perimeter (Rectangle s2 s1)

      prop "is zero only if the shape is degenerate" $ do
        forAll (arbitrary `suchThat` ((== 0) . perimeter)) $
          \shape -> shape `shouldSatisfy` isDegenerate
