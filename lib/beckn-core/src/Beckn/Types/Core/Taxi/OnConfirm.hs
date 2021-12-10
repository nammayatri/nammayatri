module Beckn.Types.Core.Taxi.OnConfirm
  ( module Beckn.Types.Core.Taxi.OnConfirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnConfirm.Order as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
