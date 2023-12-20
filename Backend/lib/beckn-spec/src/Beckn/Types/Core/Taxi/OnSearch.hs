{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch
  ( module Beckn.Types.Core.Taxi.OnSearch,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Descriptor as Reexport hiding (Descriptor (..))
import Beckn.Types.Core.Taxi.Common.Payment as Reexport
import Beckn.Types.Core.Taxi.Common.Tags as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Addon as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Catalog as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Category as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Descriptor as Reexport -- why onsearch has a seperate descriptor?
import Beckn.Types.Core.Taxi.OnSearch.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Item as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Location as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Offer as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Price as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Provider as Reexport
import Beckn.Types.Core.Taxi.OnSearch.ProviderLocation as Reexport
-- import Beckn.Types.Core.Taxi.OnSearch.StartInfo as Reexport -- to be removed
-- import Beckn.Types.Core.Taxi.OnSearch.StopInfo as Reexport -- to be removed
import Kernel.Prelude

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype OnSearchMessageV2 = OnSearchMessageV2
  { catalog :: CatalogV2
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
