{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect
  ( module Beckn.Types.Core.Taxi.OnSelect,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Payment as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Addon as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Agent as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Category as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Descriptor as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Item as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Offer as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Order as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Provider as Reexport
import Beckn.Types.Core.Taxi.OnSelect.ProviderLocation as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Quote as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Kernel.Prelude

newtype OnSelectMessage = OnSelectMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype OnSelectMessageV2 = OnSelectMessageV2
  { order :: OrderV2
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
