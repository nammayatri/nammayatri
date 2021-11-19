module Beckn.Types.Core.Migration1.OnCancel
  ( module Beckn.Types.Core.Migration1.OnCancel,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.OnCancel.CancellationSource as Reexport
import Beckn.Types.Core.Migration1.OnCancel.Order as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data OnCancelMessage = OnCancelMessage
  { order :: Order,
    cancellation_reason_id :: CancellationSource
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
