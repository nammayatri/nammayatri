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
             :<|> ( TokenAuth
                      :> Capture "rideId" (Id Ride.Ride)
                      :> "odometerReading"
                      :> QueryParam "start" Bool
                      :> ReqBody '[JSON] DRide.UploadOdometerReadingReq
                      :> Post '[JSON] APISuccess
                      :<|> TokenAuth
                      :> Capture "rideId" (Id Ride.Ride)
                      :> "odometerReading"
                      :> QueryParam "start" Bool
                      :> Get '[JSON] DRide.OdometerReadingRes
                  )
         )

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong,
    odometerStartReading :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data EndRideReq = EndRideReq
  { point :: LatLong,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    odometerEndReading :: Maybe Int,
    endRideOtp :: Maybe Text
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
    :<|> uploadOdometerReading
    :<|> getOdometerReading

startRide :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> StartRideReq -> FlowHandler APISuccess
startRide (requestorId, merchantId) rideId StartRideReq {rideOtp, point, odometerStartReading} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, odometerStartReading, requestor}
  shandle <- withTimeAPI "startRide" "buildStartRideHandle" $ RideStart.buildStartRideHandle merchantId
  withTimeAPI "startRide" "driverStartRide" $ RideStart.driverStartRide shandle rideId driverReq

otpRideCreateAndStart :: (Id SP.Person, Id Merchant.Merchant) -> DRide.OTPRideReq -> FlowHandler DRide.DriverRideRes
otpRideCreateAndStart (requestorId, merchantId) req@DRide.OTPRideReq {..} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  now <- getCurrentTime
  driverInfo <- QDI.findById (cast requestor.id) >>= fromMaybeM (PersonNotFound requestor.id.getId)
  unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
  let rideOtp = req.specialZoneOtpCode
  transporterConfig <- TC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  booking <- runInReplica $ QBooking.findBookingBySpecialZoneOTP requestor.merchantId rideOtp now transporterConfig.specialZoneBookingOtpExpiry >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp rideOtp)
  -- booking <- QBooking.findBookingBySpecialZoneOTP requestor.merchantId rideOtp now >>= fromMaybeM (BookingNotFoundForSpecialZoneOtp rideOtp)
  ride <- DRide.otpRideCreate requestor rideOtp booking
  let driverReq = RideStart.DriverStartRideReq rideOtp point Nothing requestor
  shandle <- RideStart.buildStartRideHandle merchantId
  void $ RideStart.driverStartRide shandle ride.id driverReq
  return ride

endRide :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> EndRideReq -> FlowHandler RideEnd.EndRideResp
endRide (requestorId, merchantId) rideId EndRideReq {point, uiDistanceCalculationWithAccuracy, uiDistanceCalculationWithoutAccuracy, odometerEndReading, endRideOtp} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideEnd.DriverEndRideReq {point, requestor, uiDistanceCalculationWithAccuracy, uiDistanceCalculationWithoutAccuracy, odometerEndReading, endRideOtp}
  shandle <- withTimeAPI "endRide" "buildEndRideHandle" $ RideEnd.buildEndRideHandle merchantId
  withTimeAPI "endRide" "driverEndRide" $ RideEnd.driverEndRide shandle rideId driverReq

cancelRide :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> CancelRideReq -> FlowHandler RideCancel.CancelRideResp
cancelRide (personId, _) rideId CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  let driverReq = RideCancel.CancelRideReq {reasonCode, additionalInfo}
  RideCancel.driverCancelRideHandler RideCancel.cancelRideHandle personId rideId driverReq

listDriverRides ::
  (Id SP.Person, Id Merchant.Merchant) ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe Ride.RideStatus ->
  Maybe Day ->
  FlowHandler DRide.DriverRideListRes
listDriverRides (driverId, _) mbLimit mbOffset mbRideStatus mbDay = withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset mbRideStatus mbDay

arrivedAtPickup :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup (_, _) rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req

uploadOdometerReading :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> Maybe Bool -> DRide.UploadOdometerReadingReq -> FlowHandler APISuccess
uploadOdometerReading (driverId, _) rideId isStartRide req = withFlowHandlerAPI $ do DRide.uploadOdometerReading driverId rideId isStartRide req

getOdometerReading :: (Id SP.Person, Id Merchant.Merchant) -> Id Ride.Ride -> Maybe Bool -> FlowHandler DRide.OdometerReadingRes
getOdometerReading (_, _) rideId isStartRide = withFlowHandlerAPI $ do DRide.getOdometerReading rideId isStartRide
