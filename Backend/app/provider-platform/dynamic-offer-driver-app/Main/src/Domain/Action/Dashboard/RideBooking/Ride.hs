{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideBooking.Ride
  ( postRideStart,
    postRideEnd,
    getRideCurrentActiveRide,
    postRideCancel,
    postRideBookingWithVehicleNumberAndPhone,
  )
where

import qualified API.Types.Dashboard.RideBooking.Ride
import qualified "this" API.Types.Dashboard.RideBooking.Ride as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postRideStart :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.StartRideReq -> Flow APISuccess
postRideStart merchantShortId opCity reqRideId Common.StartRideReq {point, odometerReadingValue} = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let odometer = (\value -> DRide.OdometerReading Nothing value) <$> odometerReadingValue
  let dashboardReq = SHandler.DashboardStartRideReq {point, merchantId, merchantOperatingCityId, odometer}
  shandle <- SHandler.buildStartRideHandle merchantId merchantOperatingCityId (Just rideId)
  SHandler.dashboardStartRide shandle rideId dashboardReq

postRideEnd :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.EndRideReq -> Flow APISuccess
postRideEnd merchantShortId opCity reqRideId Common.EndRideReq {point, odometerReadingValue} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  let odometer = (\value -> DRide.OdometerReading Nothing value) <$> odometerReadingValue
  let dashboardReq = EHandler.DashboardEndRideReq {point, merchantId, merchantOperatingCityId, odometer}
  shandle <- EHandler.buildEndRideHandle merchantId merchantOperatingCityId (Just rideId)
  EHandler.dashboardEndRide shandle rideId dashboardReq

getRideCurrentActiveRide :: ShortId DM.Merchant -> Context.City -> Text -> Flow (Id Common.Ride)
getRideCurrentActiveRide merchantShortId _opCity vehicleNumber = DRide.currentActiveRide merchantShortId vehicleNumber

postRideCancel :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.CancelRideReq -> Flow APISuccess
postRideCancel merchantShortId opCity reqRideId Common.CancelRideReq {reasonCode, additionalInfo} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let dashboardReq =
        CHandler.CancelRideReq
          { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reasonCode,
            additionalInfo,
            doCancellationRateBasedBlocking = Nothing
          }
  CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id merchantOpCityId rideId dashboardReq

postRideBookingWithVehicleNumberAndPhone :: ShortId DM.Merchant -> Context.City -> Common.BookingWithVehicleAndPhoneReq -> Flow Common.BookingWithVehicleAndPhoneRes
postRideBookingWithVehicleNumberAndPhone merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DRide.bookingWithVehicleNumberAndPhone merchant merchantOpCityId req
