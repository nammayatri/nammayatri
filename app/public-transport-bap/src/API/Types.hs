module API.Types where

import API.Beckn.Types as Beckn
import qualified API.Swagger.Types as Swagger
import API.UI.Types as UI
import Servant

type API =
  MainAPI
    :<|> Swagger.API

type MainAPI =
  Beckn.API
    :<|> UI.API
