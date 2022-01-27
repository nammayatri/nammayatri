module API.Types where

import API.Beckn.Types as Beckn
import API.PublicTransport.Types as PublicTransport
import Servant

type API = MainAPI

type MainAPI =
  Beckn.API
    :<|> PublicTransport.API
