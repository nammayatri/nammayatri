module Types.API.Product where

import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Traveller
import Beckn.Types.Mobility.Vehicle
import Beckn.Types.Storage.ProductInstance
import EulerHS.Prelude

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Maybe Vehicle,
    caseId :: Text,
    product :: ProductInstance,
    driver :: Maybe Driver,
    fare :: Maybe Price,
    travellers :: [Traveller]
  }
  deriving (Generic, ToJSON, FromJSON)
