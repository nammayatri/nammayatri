module Services.Endpoint where

import Prelude
import Services.Config (getBaseUrl)

nearBySearchRequest :: String -> String
nearBySearchRequest id = (getBaseUrl "") <> "/driver/nearbyRideRequest" <> if id == "" then "" else "?searchTryId=" <> id


quoteOffer :: String -> String
quoteOffer _ = (getBaseUrl "") <> "/driver/searchRequest/quote/respond"
