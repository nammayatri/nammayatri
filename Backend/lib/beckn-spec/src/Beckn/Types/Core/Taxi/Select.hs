{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Select
  ( module Beckn.Types.Core.Taxi.Select,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.BreakupItem as Reexport
import Beckn.Types.Core.Taxi.Common.Descriptor as Reexport
import Beckn.Types.Core.Taxi.Common.Tags as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Select.Order as Reexport
import Beckn.Types.Core.Taxi.Select.Quote as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SelectMessage = SelectMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype SelectMessageV2 = SelectMessageV2
  { order :: OrderV2
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
