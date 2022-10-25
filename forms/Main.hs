module Main where

import Forms

main :: IO ()
main = do
  answers <- askMultiple [whatIsYourName, howOldAreYou]
  print answers
