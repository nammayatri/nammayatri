{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder
  ( module Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Reexport
import Beckn.Types.Core.Taxi.Common.Payment as Reexport
import Beckn.Types.Core.Taxi.Common.RideCompletedQuote as Reexport
import Beckn.Types.Core.Taxi.OnStatus.Order.OrderState (RideCompletedOrderCode (RIDE_COMPLETED))
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (id, state)
import Kernel.Prelude

data RideCompletedOrder = RideCompletedOrder
  { id :: Text,
    state :: RideCompletedOrderCode,
    quote :: RideCompletedQuote,
    fulfillment :: FulfillmentInfo,
    payment :: Maybe Payment
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

orderState :: RideCompletedOrderCode
orderState = RIDE_COMPLETED
