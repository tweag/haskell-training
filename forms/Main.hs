module Main where

import Forms

main :: IO ()
main = do
  answers <- askMultiple' (AddOne ParagraphQuestion (AddOne NumberQuestion None))
  print answers
