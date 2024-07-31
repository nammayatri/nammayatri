module Components.DriverInfoCard.Common.Types where

import Prelude

import MerchantConfig.Types (AppConfig)
import Data.Maybe (Maybe(..))
import Common.Types.App as CTP
import Screens.Types (City, SearchResultType, FareProductType)

type DriverDetailsType
  = { fareProductType :: FareProductType
    , rating :: Number
    , driverName :: String
    , vehicleDetails :: String
    , vehicleVariant :: String
    , merchantCity :: City
    , registrationNumber :: String
    , config :: AppConfig
    , rideStarted :: Boolean
    , enablePaddingBottom :: Boolean
    , vehicleModel :: String
    , vehicleColor :: String
    , serviceTierName :: Maybe String
    , providerType :: CTP.ProviderType
    , showAcView :: Boolean
    , isSpecialZone :: Boolean
    }

type TripDetails a
  = { rideStarted :: Boolean
  , source :: String
  , destination :: String
  , onAnimationEnd :: a
  , backgroundColor :: String
  , enablePaddingBottom :: Boolean
  , fareProductType :: FareProductType
  , enableEditDestination :: Boolean
  , isSpecialZone :: Boolean
  , editingDestinationLoc :: a
    }
