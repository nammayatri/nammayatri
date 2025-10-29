{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Ride
  ( module Dashboard.ProviderPlatform.Management.Ride,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Ride as Reexport
import Dashboard.Common as Reexport
import Dashboard.Common.Booking as Reexport (CancellationReasonCode (..))
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Predicate (UniqueField (UniqueField))
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation

---------------------------------------------------------
-- ride list --------------------------------------------

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''RideStatus)

---------------------------------------------------------
-- multiple ride end ------------------------------

validateMultipleRideEndReq :: Validate MultipleRideEndReq
validateMultipleRideEndReq MultipleRideEndReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- multiple ride cancel ---------------------------

validateMultipleRideCancelReq :: Validate MultipleRideCancelReq
validateMultipleRideCancelReq MultipleRideCancelReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

-- ticket ride list --------------------------------------------

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

deriving anyclass instance FromJSON TicketRideListRes

deriving anyclass instance ToJSON TicketRideListRes

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase
