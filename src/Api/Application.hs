module Api.Application where

import Api.AppServices
import Api.Forms

-- servant-server
import Servant

-- warp
import Network.Wai.Handler.Warp (run)

app :: AppServices -> Application
app appServices = serve (Proxy :: Proxy (NamedRoutes FormsApi)) (formsServer appServices)

main :: IO ()
main = do
  appServices <- _
  run 8080 (app appServices)
