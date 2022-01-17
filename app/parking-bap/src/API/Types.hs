module API.Types where

import qualified API.Beckn.Types as Beckn
import qualified API.Parking.Types as Parking
import qualified API.Swagger.Types as Swagger
import Servant

type API =
  MainAPI
    :<|> Swagger.API

type MainAPI =
  Beckn.API
    :<|> Parking.API
