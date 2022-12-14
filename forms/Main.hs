{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import Data.Maybe

-- bytestring
import Data.ByteString.Char8

-- hasql
import Hasql.Connection

-- warp
import Network.Wai.Handler.Warp

import Api.Application
import Api.AppServices

main :: IO ()
main = do
  connection <- acquire "host=localhost port=5432 dbname=db user=user password=pwd"
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (run 8080 . app . postgresAppServices)
    connection
