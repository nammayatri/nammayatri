module Types.API.Confirm where


import EulerHS.Prelude
import Data.Swagger

data ConfirmReq =
  ConfirmReq
    { caseId :: Text
    , productId :: Text
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
