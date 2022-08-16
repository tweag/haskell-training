{-# LANGUAGE OverloadedStrings #-}

module Main where

import Domain.Id
import Infrastructure.Persistence

-- base
import Data.Maybe

-- bytestring
import Data.ByteString.Char8

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- uuid
import Data.UUID

main :: IO ()
main = do
  connection <- acquire "host=localhost port=5432 dbname=db user=user password=pwd"
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> do
      response <- run (statement () . select $ questionAnswers (Id nil)) connection'
      print response)
    connection
