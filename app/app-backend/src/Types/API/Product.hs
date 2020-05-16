module Types.API.Product where

import Beckn.Types.Common
import Beckn.Types.Storage.Vehicle
--import Beckn.Types.Storage.Driver
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data GetProductInfoRes = GetProductInfoRes
  { vehicle :: Vehicle
  , caseId :: Text
  , productId :: Text
  --, driver :: Driver
  }
  deriving (Generic, ToJSON, FromJSON)
