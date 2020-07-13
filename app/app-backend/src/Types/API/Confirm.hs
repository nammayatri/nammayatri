module Types.API.Confirm where

import Data.Swagger
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { caseId :: Text,
    productInstanceId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
