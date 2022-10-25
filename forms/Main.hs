module Main where

import Forms

main :: IO ()
main = do
  name <- ask whatIsYourName
  age  <- ask howOldAreYou
  putStrLn ("name: " <> show name <> "; age: " <> show age)
