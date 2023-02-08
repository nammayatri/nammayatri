module Beckn.Types.Core.Taxi.OnSearch
  ( module Beckn.Types.Core.Taxi.OnSearch,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnSearch.Addon as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Catalog as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Category as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Descriptor as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Item as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Location as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Offer as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Payment as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Price as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Provider as Reexport
import Beckn.Types.Core.Taxi.OnSearch.ProviderLocation as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StartInfo as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StopInfo as Reexport
import Kernel.Prelude

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
