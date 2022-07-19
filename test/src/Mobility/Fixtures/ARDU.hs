module Mobility.Fixtures.ARDU where

import "driver-offer-bpp" App.Routes as DrOfRoutes
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Data.Time
import qualified "app-backend" Domain.Types.Person as TPerson
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
--import qualified "driver-offer-bpp" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified "driver-offer-bpp" Types.API.Driver as DriverAPI
import "driver-offer-bpp" Types.API.Location
import qualified "driver-offer-bpp" Types.API.Ride as RideAPI

bapTransporterName :: Text
bapTransporterName = "Driver-Offer-Provider #1"

--import qualified "driver-offer-bpp" Types.API.RideBooking as TRideBookingAPI

rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> RideAPI.EndRideReq -> ClientM APISuccess
--rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd = client (Proxy :: Proxy DrOfRoutes.RideAPI)

getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes
getNearbySearchRequests :: RegToken -> ClientM DriverAPI.GetNearbySearchRequestsRes
offerQuote :: RegToken -> DriverAPI.DriverOfferReq -> ClientM APISuccess
setDriverOnline :: Text -> Bool -> ClientM APISuccess
( _
    :<|> _
    :<|> _
    :<|> _
  )
  :<|> ( setDriverOnline
           :<|> getNearbySearchRequests
           :<|> offerQuote
           :<|> ( getDriverInfo
                    :<|> _
                  )
         ) = client (Proxy :: Proxy DrOfRoutes.DriverAPI)

{-
rideRespond :: Id TRB.RideBooking -> Text -> TRideBookingAPI.SetDriverAcceptanceReq -> ClientM TRideBookingAPI.SetDriverAcceptanceRes
rideRespond rideBookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy DrOfRoutes.RideBookingAPI)
    rideResp :<|> _ = driver_rb_path rideBookingId

getNotificationInfo :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.GetRideInfoRes
getNotificationInfo rideBookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy DrOfRoutes.RideBookingAPI)
    _ :<|> getNotif = driver_rb_path rideBookingId

tRideBookingStatus :: Id TRB.RideBooking -> Text -> ClientM TRideBookingAPI.RideBookingStatusRes
tRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM TRideBookingAPI.RideBookingListRes
(tRideBookingStatus :<|> tRideBookingList :<|> _) :<|> _ = client (Proxy :: Proxy DrOfRoutes.RideBookingAPI)
-}

buildStartRideReq :: Text -> LatLong -> RideAPI.StartRideReq
buildStartRideReq otp initialPoint =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp,
      point = initialPoint
    }

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

driverToken1 :: Text
driverToken1 = "favorit-auto1-0000000000000000-token"

testDriverId1 :: Id TPerson.Person
testDriverId1 = Id "favorit-auto1-0000000000000000000000"

--
driverToken2 :: Text
driverToken2 = "favorit-sedan-0000000000000000-token"

testDriverId2 :: Id TPerson.Person
testDriverId2 = Id "favorit-sedan-0000000000000000000000"

getDriverOfferBppBaseUrl :: BaseUrl
getDriverOfferBppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8016,
      baseUrlPath = "/v2"
    }
