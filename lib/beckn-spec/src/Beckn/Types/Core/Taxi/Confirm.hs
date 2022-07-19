module Beckn.Types.Core.Taxi.Confirm
  ( module Beckn.Types.Core.Taxi.Confirm,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Confirm.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Confirm.Location as Reexport
import Beckn.Types.Core.Taxi.Confirm.Order as Reexport
import Beckn.Types.Core.Taxi.Confirm.Payment as Reexport
import Beckn.Types.Core.Taxi.Confirm.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Confirm.StopInfo as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
