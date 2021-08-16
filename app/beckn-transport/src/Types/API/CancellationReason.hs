module Types.API.CancellationReason where

import EulerHS.Prelude
import Types.Storage.CancellationReason (CancellationReasonCode)

data CancellationReasonEntity = CancellationReasonEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type ListRes = [CancellationReasonEntity]
