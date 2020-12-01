module Types.API.Cancel where

import Beckn.Types.Core.Ack
import EulerHS.Prelude

data Entity = CASE | PRODUCT_INSTANCE
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Cancel = Cancel
  { entityId :: Text,
    entityType :: Entity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelReq = CancelReq
  { transaction_id :: Text,
    message :: Cancel
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse
