module Beckn.Types.Core.Taxi.Cancel
  ( module Beckn.Types.Core.Taxi.Cancel,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Cancel.CancellationSource as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data CancelMessage = CancelMessage
  { order_id :: Text,
    cancellation_reason_id :: CancellationSource
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)