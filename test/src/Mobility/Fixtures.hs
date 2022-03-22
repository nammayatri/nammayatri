module Mobility.Fixtures where

import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Time (Seconds)
import Data.Time
import qualified "app-backend" Domain.Types.CancellationReason as AbeCRC
import qualified "app-backend" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as BQuote
import qualified "app-backend" Domain.Types.RegistrationToken as AppSRT
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "beckn-transport" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.RideBooking as BRB
import qualified "beckn-transport" Domain.Types.RideBooking as TRB
import qualified "app-backend" Domain.Types.SearchRequest as BSearchRequest
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified Types.API.Cancel as CancelAPI
import qualified Types.API.Confirm as ConfirmAPI
import qualified "beckn-transport" Types.API.Driver as DriverAPI
import qualified "app-backend" Types.API.Feedback as AppFeedback
import "beckn-transport" Types.API.Location
import qualified Types.API.Quote as QuoteAPI
import qualified "app-backend" Types.API.Registration as Reg
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "app-backend" Types.API.RideBooking as AppRideBooking
import qualified "beckn-transport" Types.API.RideBooking as TRideBookingAPI
import qualified "app-backend" Types.API.Search as AppBESearch
import qualified "app-backend" Types.API.Serviceability as AppServ

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

address :: AppBESearch.SearchReqAddress
address =
  AppBESearch.SearchReqAddress
    { door = Just "#817",
      building = Just "Juspay Apartments",
      street = Just "27th Main",
      area = Just "8th Block Koramangala",
      city = Just "Bangalore",
      country = Just "India",
      areaCode = Just "560047",
      state = Just "Karnataka"
    }

defaultAddress :: AppBESearch.SearchReqAddress
defaultAddress =
  AppBESearch.SearchReqAddress
    { door = Nothing,
      building = Nothing,
      street = Nothing,
      area = Just "Edappally",
      city = Just "Kochi",
      country = Just "India",
      areaCode = Just "",
      state = Just "Kerala"
    }

origin1 :: LatLong
origin1 = LatLong 10.0739 76.2733

searchReq :: AppBESearch.SearchReq
searchReq =
  AppBESearch.SearchReq
    { origin = AppBESearch.SearchReqLocation address origin1,
      destination = AppBESearch.SearchReqLocation address $ LatLong 10.5449 76.4356
    }

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

searchServices ::
  Text ->
  AppBESearch.SearchReq ->
  ClientM AppBESearch.SearchRes
searchServices = client (Proxy :: Proxy AbeRoutes.SearchAPI)

cancelRide :: Id BRB.RideBooking -> Text -> CancelAPI.CancelReq -> ClientM CancelAPI.CancelRes
cancelRide = client (Proxy :: Proxy AbeRoutes.CancelAPI)

rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy TbeRoutes.RideAPI)

getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes
setDriverOnline :: Text -> Bool -> ClientM APISuccess
( _
    :<|> _
    :<|> _
    :<|> _
    :<|> _
  )
  :<|> ( setDriverOnline
           :<|> ( getDriverInfo
                    :<|> _
                  )
         ) = client (Proxy :: Proxy TbeRoutes.DriverAPI)

rideRespond :: Id TRB.RideBooking -> Text -> TRideBookingAPI.SetDriverAcceptanceReq -> ClientM TRideBookingAPI.SetDriverAcceptanceRes
rideRespond rideBookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    rideResp :<|> _ = driver_rb_path rideBookingId

getNotificationInfo :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.GetRideInfoRes
getNotificationInfo rideBookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    _ :<|> getNotif = driver_rb_path rideBookingId

buildAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
buildAppCancelReq stage =
  CancelAPI.CancelReq
    { bookingCancellationReason = CancelAPI.RideBookingCancellationReasonAPIEntity (AbeCRC.CancellationReasonCode "OTHER") stage Nothing
    }

getQuotes :: Id BSearchRequest.SearchRequest -> Text -> ClientM QuoteAPI.GetQuotesRes
getQuotes = client (Proxy :: Proxy AbeRoutes.QuoteAPI)

appConfirmRide :: Text -> Id BSearchRequest.SearchRequest -> Id BQuote.Quote -> ClientM ConfirmAPI.ConfirmRes
appConfirmRide = client (Proxy :: Proxy AbeRoutes.ConfirmAPI)

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

tRideBookingStatus :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.RideBookingStatusRes
tRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM TRideBookingAPI.RideBookingListRes
(tRideBookingStatus :<|> tRideBookingList :<|> _) :<|> _ = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)

appRideBookingStatus :: Id BRB.RideBooking -> Text -> ClientM AppRideBooking.RideBookingStatusRes
appRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM AppRideBooking.RideBookingListRes
appRideBookingStatus :<|> appRideBookingList = client (Proxy :: Proxy AbeRoutes.RideBookingAPI)

buildStartRideReq :: Text -> RideAPI.StartRideReq
buildStartRideReq otp =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp
    }

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

updateLocation :: RegToken -> NonEmpty Waypoint -> ClientM APISuccess
(_ :<|> updateLocation) = client (Proxy @LocationAPI)

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty Waypoint)
buildUpdateLocationRequest pts = do
  now <- getCurrentTime
  pure $
    flip fmap pts $ \ll ->
      Waypoint
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
      mobileCountryCode = "+91"
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

getAppBaseUrl :: BaseUrl
getAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8013,
      baseUrlPath = "/v2"
    }

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
