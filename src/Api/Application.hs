module Api.Application where

import Api.AppServices
import Api.Forms

-- servant-server
import Servant

app :: AppServices -> Application
app appServices = serve (Proxy :: Proxy (NamedRoutes FormsApi)) (formsServer appServices)
