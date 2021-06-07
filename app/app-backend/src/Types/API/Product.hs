module Types.API.Product where

import Beckn.Types.Storage.ProductInstance
import EulerHS.Prelude hiding (product)
import Types.Common

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Maybe Vehicle,
    caseId :: Text,
    product :: ProductInstance,
    driver :: Maybe Driver,
    fare :: Maybe DecimalValue,
    travellers :: [Traveller]
  }
  deriving (Generic, ToJSON, FromJSON)
