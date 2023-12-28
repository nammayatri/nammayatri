{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.RideBooking.Ride where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API =
  "ride"
    :> ( Common.RideStartAPI
           :<|> Common.RideEndAPI
           :<|> Common.CurrentActiveRideAPI
           :<|> Common.RideCancelAPI
           :<|> Common.BookingWithVehicleNumberAndPhoneAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  rideStart merchantId city
    :<|> rideEnd merchantId city
    :<|> currentActiveRide merchantId city
    :<|> rideCancel merchantId city
    :<|> bookingWithVehicleNumberAndPhone merchantId city

rideStart :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId opCity reqRideId Common.StartRideReq {point} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let dashboardReq = SHandler.DashboardStartRideReq {point, merchantId, merchantOperatingCityId}
  shandle <- SHandler.buildStartRideHandle merchantId merchantOperatingCityId
  SHandler.dashboardStartRide shandle rideId dashboardReq

rideEnd :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId opCity reqRideId Common.EndRideReq {point} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  let dashboardReq = EHandler.DashboardEndRideReq {point, merchantId, merchantOperatingCityId}
  shandle <- EHandler.buildEndRideHandle merchantId merchantOperatingCityId
  EHandler.dashboardEndRide shandle rideId dashboardReq

currentActiveRide :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler (Id Common.Ride)
currentActiveRide merchantShortId _ vehicleNumber = withFlowHandlerAPI $ DRide.currentActiveRide merchantShortId vehicleNumber

rideCancel :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId opCity reqRideId Common.CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let dashboardReq =
        CHandler.CancelRideReq
          { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reasonCode,
            additionalInfo
          }
  CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id merchantOpCityId rideId dashboardReq

bookingWithVehicleNumberAndPhone :: ShortId DM.Merchant -> Context.City -> Common.BookingWithVehicleAndPhoneReq -> FlowHandler Common.BookingWithVehicleAndPhoneRes
bookingWithVehicleNumberAndPhone merchantShortId opCity req = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DRide.bookingWithVehicleNumberAndPhone merchant merchantOpCityId req
