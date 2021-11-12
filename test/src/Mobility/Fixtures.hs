module Mobility.Fixtures where

import "app-backend" App.Routes as AbeRoutes
import "beckn-transport" App.Routes as TbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Price
import Beckn.Types.Id
import Beckn.Types.Mobility.Payload
import Data.Time
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified Types.API.Cancel as CancelAPI
import qualified Types.API.Confirm as ConfirmAPI
import qualified "beckn-transport" Types.API.Driver as DriverAPI
import qualified "app-backend" Types.API.Feedback as AppFeedback
import qualified Types.API.Quote as QuoteAPI
import qualified "app-backend" Types.API.Registration as Reg
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "app-backend" Types.API.RideBooking as AppRideBooking
import qualified "beckn-transport" Types.API.RideBooking as TRideBookingAPI
import qualified "app-backend" Types.API.Search as AppBESearch
import qualified "app-backend" Types.API.Serviceability as AppServ
import qualified "app-backend" Types.Common as AppCommon
import qualified "app-backend" Types.Storage.CancellationReason as AbeCRC
import qualified "app-backend" Types.Storage.Quote as BQuote
import qualified "app-backend" Types.Storage.RegistrationToken as AppSRT
import qualified "app-backend" Types.Storage.Ride as BRide
import qualified "beckn-transport" Types.Storage.Ride as TRide
import qualified "app-backend" Types.Storage.RideBooking as BRB
import qualified "beckn-transport" Types.Storage.RideBooking as TRB
import qualified "app-backend" Types.Storage.SearchReqLocation as AppBESearchReqLoc
import qualified "app-backend" Types.Storage.SearchRequest as BSearchRequest

address :: AppCommon.Address
address =
  AppCommon.Address
    { door = "#817",
      building = "Juspay Apartments",
      street = "27th Main",
      area = "8th Block Koramangala",
      city = "Bangalore",
      country = "India",
      areaCode = "560047",
      state = "Karnataka"
    }

location :: AppCommon.GPS -> AppCommon.Location
location gps =
  AppCommon.Location
    { gps = Just gps,
      address = Just address,
      city = Nothing
    }

vehicle :: AppCommon.Vehicle
vehicle =
  AppCommon.Vehicle
    { category = Just AppCommon.CAR,
      capacity = Nothing,
      make = Nothing,
      model = Nothing,
      size = Nothing,
      variant = "SUV",
      color = Just "Black",
      registrationNumber = Nothing
    }

intentPayload :: Payload
intentPayload =
  Payload
    { travellers = [],
      traveller_count = Just 2,
      luggage = Nothing,
      travel_group = Nothing
    }

price :: Price
price =
  let amt = DecimalValue "800" Nothing
   in Price
        { currency = "INR",
          value = Just amt,
          estimated_value = Just amt,
          computed_value = Just amt,
          listed_value = Just amt,
          offered_value = Just amt,
          minimum_value = Just amt,
          maximum_value = Just amt
        }

getStop :: UTCTime -> AppCommon.GPS -> AppCommon.Stop
getStop stopTime gps =
  AppCommon.Stop
    { location = location gps,
      arrivalTime = AppCommon.StopTime stopTime (Just stopTime),
      departureTime = AppCommon.StopTime stopTime (Just stopTime)
    }

searchReq :: AppBESearch.SearchReq
searchReq =
  AppBESearch.SearchReq
    { origin = AppBESearchReqLoc.SearchReqLocationAPIEntity address $ AppCommon.GPS "10.0739" "76.2733",
      destination = AppBESearchReqLoc.SearchReqLocationAPIEntity address $ AppCommon.GPS "10.5449" "76.4356"
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
rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy TbeRoutes.RideAPI)

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
    _ :<|> (_ :<|> driver_rb_path) = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    rideResp :<|> _ = driver_rb_path rideBookingId

getNotificationInfo :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.GetRideInfoRes
getNotificationInfo rideBookingId = getNotif
  where
    _ :<|> (_ :<|> driver_rb_path) = client (Proxy :: Proxy TbeRoutes.RideBookingAPI)
    _ :<|> getNotif = driver_rb_path rideBookingId

buildAppCancelReq :: CancelAPI.CancelReq
buildAppCancelReq =
  CancelAPI.CancelReq
    { rideCancellationReason = Just $ CancelAPI.RideCancellationReasonAPIEntity (AbeCRC.CancellationReasonCode "OTHER") Nothing
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

-- buildOrgRideReq :: TQuote.ProductInstanceStatus -> TCase.CaseType -> ClientM TbePI.ProductInstanceList
-- buildOrgRideReq status csType = listOrgRides appRegistrationToken [status] [csType] (Just 50) Nothing

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

driverToken :: Text
driverToken = "ca05cf3c-c88b-4a2f-8874-191659397e0d"

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

testDriverId :: Text
testDriverId = "6bc4bc84-2c43-425d-8853-22f47bd06691"

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
      deviceToken = Just $ FCMRecipientToken "AN_DEV_TOKEN"
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
