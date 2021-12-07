module API.Types where

import qualified API.Beckn.Types as Beckn
import qualified API.Parking.Types as Parking
import Servant

type API = "v1" :> (Beckn.API :<|> Parking.API)
