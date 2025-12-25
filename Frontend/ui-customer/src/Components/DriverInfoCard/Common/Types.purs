module Components.DriverInfoCard.Common.Types where

import Prelude

import MerchantConfig.Types (AppConfig)
import Data.Maybe (Maybe(..))
import Common.Types.App as CTP
import Screens.Types (SearchResultType, FareProductType, PersonDeliveryDetails)

type DriverDetailsType
  = { fareProductType :: FareProductType
    , rating :: Number
    , driverName :: String
    , vehicleDetails :: String
    , vehicleVariant :: String
    , merchantCity :: CTP.City
    , registrationNumber :: String
    , config :: AppConfig
    , rideStarted :: Boolean
    , enablePaddingBottom :: Boolean
    , vehicleModel :: String
    , vehicleColor :: String
    , serviceTierName :: Maybe String
    , providerType :: CTP.ProviderType
    , showAcView :: Boolean
    , isAirConditioned :: Maybe Boolean
    , isOtpRideFlow :: Boolean
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
  , isOtpRideFlow :: Boolean
  , editingDestinationLoc :: a
  , rideAccepted :: Boolean
  , editingPickupLocation :: a
  , isEditPickupEnabled :: Boolean
  , senderDetails :: Maybe PersonDeliveryDetails
  , receiverDetails :: Maybe PersonDeliveryDetails
    }
