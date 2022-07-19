module Beckn.Types.Core.Taxi.OnSelect
  ( module Beckn.Types.Core.Taxi.OnSelect,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnSelect.Addon as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Category as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Descriptor as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Item as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Location as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Offer as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Order as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Payment as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Price as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Provider as Reexport
import Beckn.Types.Core.Taxi.OnSelect.ProviderLocation as Reexport
import Beckn.Types.Core.Taxi.OnSelect.StartInfo as Reexport
import Beckn.Types.Core.Taxi.OnSelect.StopInfo as Reexport

newtype OnSelectMessage = OnSelectMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
