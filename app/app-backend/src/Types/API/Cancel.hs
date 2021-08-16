module Types.API.Cancel where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude
import Types.Storage.CancellationReason (CancellationReasonCode)

data Entity = CASE | PRODUCT_INSTANCE
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data CancelReq = CancelReq
  { entityId :: Text,
    entityType :: Entity,
    cancellationReason :: Maybe RideCancellationReasonEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = APISuccess

data RideCancellationReasonEntity = RideCancellationReasonEntity
  { reasonCode :: CancellationReasonCode,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)