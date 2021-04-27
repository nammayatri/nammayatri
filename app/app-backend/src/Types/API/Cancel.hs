module Types.API.Cancel where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude

data Entity = CASE | PRODUCT_INSTANCE
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data CancelReq = CancelReq
  { entityId :: Text,
    entityType :: Entity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = APISuccess
