{-# LANGUAGE TypeApplications #-}

module Mobility.Fixtures where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import qualified "beckn-transport" API.UI.Driver as TbeDriverAPI
import qualified "beckn-transport" API.UI.Location as TbeLocation
import qualified "beckn-transport" API.UI.Ride as TbeRideAPI
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Time (Seconds)
import Data.Time
import qualified "beckn-transport" Domain.Types.Ride as TRide
import qualified "beckn-transport" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

rideStart :: Text -> Id TRide.Ride -> TbeRideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> TbeRideAPI.EndRideReq -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> TbeRideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy TbeRideAPI.API)

getDriverInfo :: Text -> ClientM TbeDriverAPI.DriverInformationRes
setDriverOnline :: Text -> Bool -> ClientM APISuccess
( _
    :<|> _
    :<|> _
    :<|> _
  )
  :<|> ( setDriverOnline
           :<|> _
           :<|> ( getDriverInfo
                    :<|> _
                  )
         ) = client (Proxy :: Proxy TbeDriverAPI.API)

rideRespond :: Id TRB.Booking -> Text -> TbeBookingAPI.SetDriverAcceptanceReq -> ClientM TbeBookingAPI.SetDriverAcceptanceRes
rideRespond bookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeBookingAPI.API)
    rideResp :<|> _ = driver_rb_path bookingId

getNotificationInfo :: Id TRB.Booking -> Text -> ClientM TbeBookingAPI.GetRideInfoRes
getNotificationInfo bookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeBookingAPI.API)
    _ :<|> getNotif = driver_rb_path bookingId

mkAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
mkAppCancelReq stage =
  CancelAPI.CancelReq (AbeCRC.CancellationReasonCode "OTHER") stage Nothing

appConfirmRide :: Text -> Id AbeQuote.Quote -> DConfirm.ConfirmAPIReq -> ClientM DConfirm.ConfirmRes
appConfirmRide = client (Proxy :: Proxy ConfirmAPI.ConfirmAPI)

confirmAddress :: DConfirm.ConfirmLocationAPIEntity
confirmAddress =
  DConfirm.ConfirmLocationAPIEntity
    { door = Just "#817",
      building = Just "Juspay Apartments",
      street = Just "27th Main",
      area = Just "8th Block Koramangala",
      city = Just "Bangalore",
      country = Just "India",
      areaCode = Just "560047",
      state = Just "Karnataka"
    }

mkAppConfirmReq :: DConfirm.ConfirmAPIReq
mkAppConfirmReq =
  DConfirm.ConfirmAPIReq
    { fromLocation = confirmAddress,
      toLocation = Just confirmAddress
    }

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback = client (Proxy :: Proxy AbeRoutes.FeedbackAPI)

tBookingStatus :: Id TRB.Booking -> Text -> ClientM TRB.BookingAPIEntity
tBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM TbeBookingAPI.BookingListRes
(tBookingStatus :<|> tBookingList :<|> _) :<|> _ = client (Proxy :: Proxy TbeBookingAPI.API)

buildStartRideReq :: Text -> LatLong -> TbeRideAPI.StartRideReq
buildStartRideReq otp pt =
  TbeRideAPI.StartRideReq
    { TbeRideAPI.rideOtp = otp,
      TbeRideAPI.point = pt
    }

updateLocation :: RegToken -> NonEmpty TbeLocation.Waypoint -> ClientM APISuccess
(_ :<|> updateLocation) = client (Proxy @TbeLocation.API)

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty TbeLocation.Waypoint)
buildUpdateLocationRequest pts = do
  now <- getCurrentTime
  pure $
    flip fmap pts $ \ll ->
      TbeLocation.Waypoint
        { pt = ll,
          ts = now,
          acc = Nothing
        }

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

driverToken1 :: Text
driverToken1 = "ca05cf3c-c88b-4a2f-8874-drivertoken1"

driverToken2 :: Text
driverToken2 = "ca05cf3c-c88b-4a2f-8874-drivertoken2"

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
