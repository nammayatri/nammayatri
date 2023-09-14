{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.APICalls where

import qualified "dynamic-offer-driver-app" API.Dashboard as DashboardAPI
import qualified "dynamic-offer-driver-app" API.UI.Driver as DriverAPI
import "dynamic-offer-driver-app" API.UI.Location as LocationAPI
import qualified "dynamic-offer-driver-app" API.UI.Ride as RideAPI
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Dashboard
import Data.Time
import qualified Domain.Action.UI.Ride.CancelRide as DCR
import qualified Domain.Action.UI.Ride.EndRide as DER
import qualified "dynamic-offer-driver-app" Domain.Types.DriverInformation as TDI
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as TDM
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as TRide
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Servant hiding (Context)
import Servant.Client

data UIAPIs = UIAPIs
  { healthCheck :: ClientM Text,
    ride :: RideAPIs,
    driver :: DriverAPIs,
    location :: LocationAPIs
  }

data RideAPIs = RideAPIs
  { rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess,
    rideEnd :: Text -> Id TRide.Ride -> RideAPI.EndRideReq -> ClientM DER.EndRideResp,
    rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM DCR.CancelRideResp
  }

data DriverAPIs = DriverAPIs
  { getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes,
    getNearbySearchRequests :: RegToken -> ClientM DriverAPI.GetNearbySearchRequestsRes,
    offerQuote :: RegToken -> DriverAPI.DriverOfferReq -> ClientM APISuccess,
    respondQuote :: RegToken -> DriverAPI.DriverRespondReq -> ClientM APISuccess,
    setDriverOnline :: Text -> Bool -> Maybe TDI.DriverMode -> ClientM APISuccess,
    updateMetaData :: RegToken -> DriverAPI.MetaDataReq -> ClientM APISuccess,
    validate :: Text -> DriverAPI.DriverAlternateNumberReq -> ClientM DriverAPI.DriverAlternateNumberRes,
    verifyAuth :: Text -> DriverAPI.DriverAlternateNumberOtpReq -> ClientM APISuccess,
    resendOtp :: Text -> DriverAPI.DriverAlternateNumberReq -> ClientM DriverAPI.ResendAuth,
    remove :: Text -> ClientM APISuccess
  }

newtype LocationAPIs = LocationAPIs
  { updateLocation :: RegToken -> NonEmpty LocationAPI.Waypoint -> ClientM APISuccess
  }

-- most of apis do not used in tests, so let's simplify API type
type HealthCheckAPI = Get '[JSON] Text

type UIAPI =
  "ui"
    :> ( HealthCheckAPI
           :<|> DriverAPI.API
           :<|> LocationAPI.API
           :<|> RideAPI.API
       )

ui :: UIAPIs
ui = do
  let ride = RideAPIs {..}
  let driver = DriverAPIs {..}
  let location = LocationAPIs {..}
  UIAPIs {..}
  where
    healthCheck
      :<|> driverClient
      :<|> locationClient
      :<|> rideClient = client (Proxy :: Proxy UIAPI)

    _ :<|> (_ :<|> _ :<|> rideStart :<|> rideEnd :<|> rideCancel) = rideClient

    ( _
        :<|> _
        :<|> _
        :<|> _
      )
      :<|> ( setDriverOnline
               :<|> _
               :<|> getNearbySearchRequests
               :<|> offerQuote
               :<|> respondQuote
               :<|> ( getDriverInfo
                        :<|> _
                        :<|> _
                      )
               :<|> updateMetaData
               :<|> ( validate
                        :<|> verifyAuth
                        :<|> resendOtp
                        :<|> remove
                      )
               :<|> _
             ) = driverClient

    (_ :<|> updateLocation) = locationClient

newtype DashboardAPIs = DashboardAPIs
  { ride :: DashboardRideAPIs
  }

newtype DashboardRideAPIs = DashboardRideAPIs
  { rideSync :: Id Dashboard.Ride -> ClientM Dashboard.RideSyncRes
  }

newtype DashboardMultipleRideAPIs = DashboardMultipleRideAPIs
  { multipleRideSync :: Dashboard.MultipleRideSyncReq -> ClientM Dashboard.MultipleRideSyncRes
  }

dashboard :: ShortId TDM.Merchant -> Text -> DashboardAPIs
dashboard merchantId token = do
  let ride = DashboardRideAPIs {..}
  DashboardAPIs {..}
  where
    helperAPIClient :<|> _exotelAPIClient = client (Proxy :: Proxy DashboardAPI.API)

    _ :<|> rideClient :<|> _ :<|> _ = helperAPIClient merchantId token

    _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> rideSync :<|> _ = rideClient

buildStartRideReq :: Text -> LatLong -> RideAPI.StartRideReq
buildStartRideReq otp initialPoint =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp,
      point = initialPoint
    }

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty LocationAPI.Waypoint)
buildUpdateLocationRequest pts =
  forM pts $ \ll -> do
    now <- getCurrentTime
    return $
      LocationAPI.Waypoint
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
      baseUrlPath = ""
    }
