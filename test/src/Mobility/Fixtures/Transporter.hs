module Mobility.Fixtures.Transporter where

--import "beckn-transport" App.Routes as TbeRoutes

--import qualified "beckn-transport" Types.API.Driver as DriverAPI

import qualified "beckn-transport" API.UI.Booking as BookingAPI
import qualified "beckn-transport" API.UI.Driver as DriverAPI
import "beckn-transport" API.UI.Location
import qualified "beckn-transport" API.UI.Location as LocationAPI
import qualified "beckn-transport" API.UI.Ride as RideAPI
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Data.Time
import qualified "beckn-transport" Domain.Types.Booking.API as BookingAPI
import qualified "app-backend" Domain.Types.Person as TPerson
import qualified "beckn-transport" Domain.Types.Ride as TRide
import qualified "beckn-transport" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy RideAPI.API)

getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes
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
         ) = client (Proxy :: Proxy DriverAPI.API)

rideRespond :: Id TRB.RideBooking -> Text -> BookingAPI.SetDriverAcceptanceReq -> ClientM BookingAPI.SetDriverAcceptanceRes
rideRespond rideBookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy BookingAPI.API)
    rideResp :<|> _ = driver_rb_path rideBookingId

getNotificationInfo :: Id TRB.RideBooking -> Text -> ClientM BookingAPI.GetRideInfoRes
getNotificationInfo rideBookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy BookingAPI.API)
    _ :<|> getNotif = driver_rb_path rideBookingId

tRideBookingStatus :: Id TRB.RideBooking -> Text -> ClientM BookingAPI.RideBookingAPIEntity
tRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM BookingAPI.RideBookingListRes
(tRideBookingStatus :<|> tRideBookingList :<|> _) :<|> _ = client (Proxy :: Proxy BookingAPI.API)

buildStartRideReq :: Text -> RideAPI.StartRideReq
buildStartRideReq otp =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp
    }

updateLocation :: RegToken -> NonEmpty Waypoint -> ClientM APISuccess
(_ :<|> updateLocation) = client (Proxy @LocationAPI.API)

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
driverToken1 = "ca05cf3c-c88b-4a2f-8874-drivertoken1"

driverToken2 :: Text
driverToken2 = "ca05cf3c-c88b-4a2f-8874-drivertoken2"

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

testDriverId1 :: Id TPerson.Person
testDriverId1 = Id "6bc4bc84-2c43-425d-8853-22f47driver1"

testDriverId2 :: Id TPerson.Person
testDriverId2 = Id "6bc4bc84-2c43-425d-8853-22f47driver2"

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
