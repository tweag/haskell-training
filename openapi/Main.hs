module Main where

import Api.Forms

-- aeson-pretty
import Data.Aeson.Encode.Pretty

-- base
import Data.Proxy

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL8

-- servant
import Servant.API

-- servant-openapi3
import Servant.OpenApi

main :: IO ()
main = do
  BL8.putStrLn . encodePretty $ toOpenApi (Proxy :: Proxy (NamedRoutes FormsApi))