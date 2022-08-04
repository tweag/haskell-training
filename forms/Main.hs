module Main where

import Forms

main :: IO ()
main = do
  answers <- askMultiple [whatIsYourName, howOldAreYou]
  print answers

-- import qualified Api.Application as Forms

-- main :: IO ()
-- main = Forms.main
