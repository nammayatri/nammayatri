module Types.API.Cancel where

import Beckn.Types.APISuccess (APISuccess)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Storage.CancellationReason (CancellationReasonCode)

newtype CancelReq = CancelReq
  { rideCancellationReason :: RideCancellationReasonAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type CancelRes = APISuccess

data RideCancellationReasonAPIEntity = RideCancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
