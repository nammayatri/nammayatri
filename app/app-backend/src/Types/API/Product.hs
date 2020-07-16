module Types.API.Product where

import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Traveller
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Products
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Maybe Vehicle,
    caseId :: Text,
    product :: ProductInstance,
    driver :: Maybe TripDriver,
    fare :: Maybe Price,
    travellers :: [Traveller]
  }
  deriving (Generic, ToJSON, FromJSON)
