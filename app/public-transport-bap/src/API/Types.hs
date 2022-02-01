module API.Types where

import API.Beckn.Types as Beckn
import API.PublicTransport.Types as PublicTransport
import qualified API.Swagger.Types as Swagger
import Servant

type API =
  MainAPI
    :<|> Swagger.API

type MainAPI =
  Beckn.API
    :<|> PublicTransport.API
