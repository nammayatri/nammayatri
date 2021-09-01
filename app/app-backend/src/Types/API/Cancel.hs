module Types.API.Cancel where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude
import Types.Storage.CancellationReason (CancellationReasonCode)

newtype CancelReq = CancelReq
  { rideCancellationReason :: Maybe RideCancellationReasonEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = APISuccess

data RideCancellationReasonEntity = RideCancellationReasonEntity
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)