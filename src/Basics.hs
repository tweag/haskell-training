module Basics where

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

perimeter :: Shape -> Float
perimeter (Rectangle side1 side2) = (side1 + side2) * 2
perimeter (Square side)           = side * 4
perimeter (Circle radius)         = 2 * pi * radius
