module Services.Endpoint where

import Prelude
import Services.Config (getBaseUrl)

nearBySearchRequest :: String
nearBySearchRequest = (getBaseUrl "") <> "/driver/nearbyRideRequest"
