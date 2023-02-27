{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Init
  ( module Beckn.Types.Core.Taxi.Init,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Init.Descriptor as Reexport
import Beckn.Types.Core.Taxi.Init.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Init.Location as Reexport
import Beckn.Types.Core.Taxi.Init.Order as Reexport
import Beckn.Types.Core.Taxi.Init.Payment as Reexport
import Beckn.Types.Core.Taxi.Init.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Init.StopInfo as Reexport
import Beckn.Types.Core.Taxi.Search.Time as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype InitMessage = InitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
