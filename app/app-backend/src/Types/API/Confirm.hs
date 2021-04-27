module Types.API.Confirm where

import Beckn.Types.APISuccess (APISuccess)
import Data.Swagger
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { caseId :: Text,
    productInstanceId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type ConfirmRes = APISuccess
