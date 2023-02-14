 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.BookingCancellationReason where

import Domain.Types.Booking (Booking)
import Domain.Types.CancellationReason (CancellationReasonCode, CancellationStage)
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Id

data BookingCancellationReason = BookingCancellationReason
  { bookingId :: Id Booking,
    rideId :: Maybe (Id Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    reasonStage :: Maybe CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Ord, Read, Generic)
