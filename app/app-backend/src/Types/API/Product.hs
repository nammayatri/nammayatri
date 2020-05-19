module Types.API.Product where

import Beckn.Types.Common
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Vehicle
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Traveller
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Maybe Vehicle
  , caseId :: Text
  , productId :: Text
  , driver :: TripDriver
  , fare :: Maybe Price
  , travellers :: [Traveller]
  }
  deriving (Generic, ToJSON, FromJSON)
