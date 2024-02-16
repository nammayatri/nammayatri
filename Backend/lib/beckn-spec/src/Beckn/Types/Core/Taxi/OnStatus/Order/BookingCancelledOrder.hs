{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder
  ( module Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Reexport
import Beckn.Types.Core.Taxi.OnStatus.Order.OrderState (RideBookingCancelledOrderCode (RIDE_BOOKING_CANCELLED))
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import Kernel.Prelude
import Kernel.Utils.Schema

data BookingCancelledOrder = BookingCancelledOrder
  { id :: Text,
    state :: RideBookingCancelledOrderCode,
    cancellation_reason :: CancellationSource,
    fulfillment :: Maybe FulfillmentInfo
  }
  deriving (Generic, Show)

orderState :: RideBookingCancelledOrderCode
orderState = RIDE_BOOKING_CANCELLED

instance ToSchema BookingCancelledOrder where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions bookingCancelledOrderJSONOptions

instance FromJSON BookingCancelledOrder where
  parseJSON = genericParseJSON bookingCancelledOrderJSONOptions

instance ToJSON BookingCancelledOrder where
  toJSON = genericToJSON bookingCancelledOrderJSONOptions

bookingCancelledOrderJSONOptions :: A.Options
bookingCancelledOrderJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "cancellation_reason" -> "./komn/cancellation_reason"
        a -> a
    }
