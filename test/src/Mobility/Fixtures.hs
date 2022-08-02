{-# LANGUAGE TypeApplications #-}

module Mobility.Fixtures where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import qualified "beckn-transport" API.UI.Driver as TbeDriverAPI
import qualified "beckn-transport" API.UI.Location as TbeLocation
import qualified "beckn-transport" API.UI.Ride as TbeRideAPI
import "app-backend" App.Routes as AbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Time (Seconds)
import Data.Time
import qualified "app-backend" Domain.Action.UI.Cancel as CancelAPI
import qualified "app-backend" Domain.Types.Booking as BRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "app-backend" Domain.Types.CancellationReason as AbeCRC
import qualified "app-backend" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as AbeQuote
import qualified "app-backend" Domain.Types.RegistrationToken as AppSRT
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import qualified "app-backend" Product.Cancel as CancelAPI
import qualified "app-backend" Product.Confirm as ConfirmAPI
import qualified "app-backend" Product.Confirm as DConfirm
import Servant hiding (Context)
import Servant.Client
import qualified "app-backend" Types.API.Booking as AppBooking
import qualified "app-backend" Types.API.Feedback as AppFeedback
import qualified "app-backend" Types.API.Registration as Reg
import qualified "app-backend" Types.API.Serviceability as AppServ

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

cancelRide :: Id BRB.Booking -> Text -> CancelAPI.CancelReq -> ClientM APISuccess
cancelRide = client (Proxy :: Proxy CancelAPI.CancelAPI)

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

appConfirmRide :: Text -> Id AbeQuote.Quote -> DConfirm.ConfirmReq -> ClientM DConfirm.ConfirmRes
appConfirmRide = client (Proxy :: Proxy ConfirmAPI.ConfirmAPI)

confirmAddress :: DConfirm.ConfirmLocationReq
confirmAddress =
  DConfirm.ConfirmLocationReq
    { door = Just "#817",
      building = Just "Juspay Apartments",
      street = Just "27th Main",
      area = Just "8th Block Koramangala",
      city = Just "Bangalore",
      country = Just "India",
      areaCode = Just "560047",
      state = Just "Karnataka"
    }

mkAppConfirmReq :: DConfirm.ConfirmReq
mkAppConfirmReq =
  DConfirm.ConfirmReq
    { fromLocation = confirmAddress,
      mbToLocation = Just confirmAddress
    }

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback = client (Proxy :: Proxy AbeRoutes.FeedbackAPI)

callAppFeedback :: Int -> Id BRide.Ride -> ClientM APISuccess
callAppFeedback ratingValue rideId =
  let request =
        AppFeedback.FeedbackReq
          { rideId = rideId,
            rating = ratingValue
          }
   in appFeedback appRegistrationToken request

tBookingStatus :: Id TRB.Booking -> Text -> ClientM TRB.BookingAPIEntity
tBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM TbeBookingAPI.BookingListRes
(tBookingStatus :<|> tBookingList :<|> _) :<|> _ = client (Proxy :: Proxy TbeBookingAPI.API)

appBookingStatus :: Id BRB.Booking -> Text -> ClientM AppBooking.BookingStatusRes
appBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM AppBooking.BookingListRes
appBookingStatus :<|> appBookingList = client (Proxy :: Proxy AbeRoutes.BookingAPI)

buildStartRideReq :: Text -> LatLong -> TbeRideAPI.StartRideReq
buildStartRideReq otp pt =
  TbeRideAPI.StartRideReq
    { TbeRideAPI.rideOtp = otp,
      TbeRideAPI.point = pt
    }

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

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

testDriverId1 :: Id TPerson.Person
testDriverId1 = Id "6bc4bc84-2c43-425d-8853-22f47driver1"

testDriverId2 :: Id TPerson.Person
testDriverId2 = Id "6bc4bc84-2c43-425d-8853-22f47driver2"

appAuth :: Reg.AuthReq -> ClientM Reg.AuthRes
appVerify :: Id AppSRT.RegistrationToken -> Reg.AuthVerifyReq -> ClientM Reg.AuthVerifyRes
appReInitiateLogin :: Id AppSRT.RegistrationToken -> ClientM Reg.ResendAuthRes
logout :: RegToken -> ClientM APISuccess
appAuth
  :<|> appVerify
  :<|> appReInitiateLogin
  :<|> logout =
    client (Proxy :: Proxy AbeRoutes.RegistrationAPI)

mkAuthReq :: Reg.AuthReq
mkAuthReq =
  Reg.AuthReq
    { mobileNumber = "9000090000",
      mobileCountryCode = "+91",
      merchantId = "FIXME"
    }

mkAuthVerifyReq :: Reg.AuthVerifyReq
mkAuthVerifyReq =
  Reg.AuthVerifyReq
    { otp = "7891",
      deviceToken = FCMRecipientToken "AN_DEV_TOKEN"
    }

initiateAuth :: ClientM Reg.AuthRes
initiateAuth = appAuth mkAuthReq

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
