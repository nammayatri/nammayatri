module Types.API.Cancel where

import Beckn.Types.APISuccess (APISuccess)
import Data.OpenApi (ToSchema)
import Domain.Types.CancellationReason (CancellationReasonCode, CancellationStage)
import EulerHS.Prelude

newtype CancelReq = CancelReq
  { bookingCancellationReason :: RideBookingCancellationReasonAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type CancelRes = APISuccess

data RideBookingCancellationReasonAPIEntity = RideBookingCancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    reasonStage :: CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
