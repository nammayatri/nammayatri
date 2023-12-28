{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order (Order (..)) where

import Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder
import Beckn.Types.Core.Taxi.OnStatus.Order.BookingReallocationOrder
import Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder
import Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder
import Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder
import Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder
import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude hiding (id)
import qualified Kernel.Utils.JSON as J
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)
import qualified Kernel.Utils.Schema as S

data Order
  = NewBooking NewBookingOrder
  | RideAssigned RideAssignedOrder
  | RideStarted RideStartedOrder
  | RideCompleted RideCompletedOrder
  | BookingCancelled BookingCancelledOrder
  | BookingReallocation BookingReallocationOrder
  deriving (Generic, Show)

instance ToJSON Order where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON Order where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema S.untaggedValue
