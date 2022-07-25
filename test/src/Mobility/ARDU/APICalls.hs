module Mobility.ARDU.APICalls where

import "driver-offer-bpp" App.Routes as DrOfRoutes
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Data.Time
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
import EulerHS.Prelude
import Servant hiding (Context)
import Servant.Client
import qualified "driver-offer-bpp" Types.API.Driver as DriverAPI
import "driver-offer-bpp" Types.API.Location
import qualified "driver-offer-bpp" Types.API.Ride as RideAPI

rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> RideAPI.EndRideReq -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy DrOfRoutes.RideAPI)

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

getDriverOfferBppBaseUrl :: BaseUrl
getDriverOfferBppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8016,
      baseUrlPath = "/ui"
    }
