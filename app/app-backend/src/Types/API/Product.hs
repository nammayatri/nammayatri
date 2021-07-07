module Types.API.Product where

import EulerHS.Prelude hiding (product)
import Types.Common
import Types.Storage.ProductInstance

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Maybe Vehicle,
    caseId :: Text,
    product :: ProductInstance,
    driver :: Maybe Driver,
    fare :: Maybe DecimalValue,
    travellers :: [Traveller]
  }
  deriving (Generic, ToJSON, FromJSON)
