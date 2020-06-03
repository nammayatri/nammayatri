module Types.API.Confirm where

import Data.Swagger
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { caseId :: Text,
    productId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
