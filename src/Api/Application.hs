{-# LANGUAGE OverloadedStrings #-}

module Api.Application where

import Api.AppServices
import Api.Forms

-- base
import Data.Maybe
import Data.Proxy

-- bytestring
import Data.ByteString.Char8

-- hasql
import Hasql.Connection

-- servant-server
import Servant

-- warp
import Network.Wai.Handler.Warp

app :: AppServices -> Application
app appServices = serve (Proxy :: Proxy (NamedRoutes FormsApi)) (formsServer appServices)

main :: IO ()
main = do
  connection <- acquire "host=postgres port=5432 dbname=db user=user password=pwd"
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (run 8080 . app . postgresAppServices)
    connection
