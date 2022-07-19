module Beckn.Types.Core.Taxi.OnConfirm
  ( module Beckn.Types.Core.Taxi.OnConfirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnConfirm.BreakupItem as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Descriptor as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Location as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Order as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Payment as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Quote as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.StartInfo as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.StopInfo as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
