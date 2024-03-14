module Components.DriverInfoCard.Common.Types where

import Prelude

import MerchantConfig.Types (AppConfig)
import Screens.Types (City, SearchResultType)
import Common.Types.App as CTP

type DriverDetailsType
  = { searchType :: SearchResultType
  , rating :: Number
  , driverName :: String
  , vehicleDetails :: String
  , vehicleVariant :: String
  , merchantCity :: City
  , registrationNumber :: String
  , config :: AppConfig
  , rideStarted :: Boolean
  , enablePaddingBottom :: Boolean
  , providerType :: CTP.ProviderType
    }

type TripDetails a
  = { rideStarted :: Boolean
  , source :: String
  , destination :: String
  , onAnimationEnd :: a
  , backgroundColor :: String
  , enablePaddingBottom :: Boolean
    }
