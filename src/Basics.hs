{-# LANGUAGE InstanceSigs #-}

module Basics where

-- QuickCheck
import Test.QuickCheck

x :: String
x = "hello"

f :: Bool -> Bool
f x = x && x

data Person = MkPerson
  { name :: String
  , age  :: Int
  }

personDetails :: Person -> String
personDetails (MkPerson name age) = name ++ " is " ++ show age ++ " years old"

data Person1 = MkPerson1 String Int

data Shape
  = Rectangle {side1 :: Float, side2 :: Float}
  | Square {side :: Float}
  | Circle {radius :: Float}
  deriving Show

instance Arbitrary Shape where
  arbitrary :: Gen Shape
  arbitrary = oneof
    [ Rectangle <$> arbitrary <*> arbitrary
    , Square <$> arbitrary
    , Circle <$> arbitrary
    ]

-- | Perimeter of a shape
--
-- >>> perimeter (Rectangle 1 2)
-- 6.0
--
-- >>> perimeter (Square 1)
-- 4.0
--
-- >>> perimeter (Circle 1)
-- 6.2831855
perimeter :: Shape -> Float
perimeter (Rectangle side1 side2) = (side1 + side2) * 2
perimeter (Square side)           = side * 4
perimeter (Circle radius)         = 2 * pi * radius

isDegenerate :: Shape -> Bool
isDegenerate (Rectangle s1 s2) = s1 == 0 && s2 == 0
isDegenerate (Square s)        = s == 0
isDegenerate (Circle r)        = r == 0
