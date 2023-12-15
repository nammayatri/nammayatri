{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Ride
  ( StartRideReq (..),
    EndRideReq (..),
    CancelRideReq (..),
    DRide.DriverRideListRes (..),
    DRide.DriverRideRes (..),
    DRide.OTPRideReq (..),
    API,
    handler,
    otpRideCreateAndStart,
  )
where

import Data.Time (Day)
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import Servant hiding (throwError)
import SharedLogic.Person (findPerson)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import Tools.Auth
import Tools.Error

type API =
  "driver"
    :> "otpRide"
    :> TokenAuth
    :> "start"
    :> ReqBody '[JSON] DRide.OTPRideReq
    :> Post '[JSON] DRide.DriverRideRes
    :<|> "driver"
      :> ( "ride"
             :> ( "list"
                    :> TokenAuth
                    :> QueryParam "limit" Integer
                    :> QueryParam "offset" Integer
                    :> QueryParam "onlyActive" Bool
                    :> QueryParam "status" Ride.RideStatus
                    :> QueryParam "day" Day
                    :> Get '[JSON] DRide.DriverRideListRes
                    :<|> TokenAuth
                    :> Capture "rideId" (Id Ride.Ride)
                    :> "arrived"
                    :> "pickup"
                    :> ReqBody '[JSON] LatLong
                    :> Post '[JSON] APISuccess
                    :<|> TokenAuth
                    :> Capture "rideId" (Id Ride.Ride)
                    :> "start"
                    :> ReqBody '[JSON] StartRideReq
                    :> Post '[JSON] APISuccess
                    :<|> TokenAuth
                    :> Capture "rideId" (Id Ride.Ride)
                    :> "end"
                    :> ReqBody '[JSON] EndRideReq
                    :> Post '[JSON] RideEnd.EndRideResp
                    :<|> TokenAuth
                    :> Capture "rideId" (Id Ride.Ride)
                    :> "cancel"
                    :> ReqBody '[JSON] CancelRideReq
                    :> Post '[JSON] RideCancel.CancelRideResp
                )
         )

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data EndRideReq = EndRideReq
  { point :: LatLong,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

handler :: FlowServer API
handler =
  otpRideCreateAndStart
    :<|> ( listDriverRides
             :<|> arrivedAtPickup
             :<|> startRide
             :<|> endRide
             :<|> cancelRide
         )

startRide :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Ride.Ride -> StartRideReq -> FlowHandler APISuccess
startRide (requestorId, merchantId, merchantOpCityId) rideId StartRideReq {rideOtp, point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- withTimeAPI "startRide" "buildStartRideHandle" $ RideStart.buildStartRideHandle merchantId merchantOpCityId
  withTimeAPI "startRide" "driverStartRide" $ RideStart.driverStartRide shandle rideId driverReq

otpRideCreateAndStart :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DRide.OTPRideReq -> FlowHandler DRide.DriverRideRes
otpRideCreateAndStart (requestorId, merchantId, merchantOpCityId) req@DRide.OTPRideReq {..} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  now <- getCurrentTime
  driverInfo <- QDI.findById (cast requestor.id) >>= fromMaybeM (PersonNotFound requestor.id.getId)
  unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
  let rideOtp = req.specialZoneOtpCode
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  booking <- runInReplica $ QBooking.findBookingBySpecialZoneOTP requestor.merchantId rideOtp now transporterConfig.specialZoneBookingOtpExpiry >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp rideOtp)
  -- booking <- QBooking.findBookingBySpecialZoneOTP requestor.merchantId rideOtp now >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp rideOtp)
  ride <- DRide.otpRideCreate requestor rideOtp booking
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- RideStart.buildStartRideHandle merchantId merchantOpCityId
  void $ RideStart.driverStartRide shandle ride.id driverReq
  return ride

endRide :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Ride.Ride -> EndRideReq -> FlowHandler RideEnd.EndRideResp
endRide (requestorId, merchantId, merchantOpCityId) rideId EndRideReq {point, uiDistanceCalculationWithAccuracy, uiDistanceCalculationWithoutAccuracy} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideEnd.DriverEndRideReq {point, requestor, uiDistanceCalculationWithAccuracy, uiDistanceCalculationWithoutAccuracy}
  shandle <- withTimeAPI "endRide" "buildEndRideHandle" $ RideEnd.buildEndRideHandle merchantId merchantOpCityId
  withTimeAPI "endRide" "driverEndRide" $ RideEnd.driverEndRide shandle rideId driverReq

cancelRide :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Ride.Ride -> CancelRideReq -> FlowHandler RideCancel.CancelRideResp
cancelRide (personId, _, _) rideId CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  let driverReq = RideCancel.CancelRideReq {reasonCode, additionalInfo}
  RideCancel.driverCancelRideHandler RideCancel.cancelRideHandle personId rideId driverReq

listDriverRides ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe Ride.RideStatus ->
  Maybe Day ->
  FlowHandler DRide.DriverRideListRes
listDriverRides (driverId, _, _) mbLimit mbOffset mbRideStatus mbDay = withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset mbRideStatus mbDay

arrivedAtPickup :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup (_, _, _) rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req
