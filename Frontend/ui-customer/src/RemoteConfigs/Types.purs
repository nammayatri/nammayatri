module RemoteConfig.Types where

import Prelude
import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode)
import Presto.Core.Utils.Encoding (defaultDecode)
import Data.Maybe (Maybe)
import Language.Types(STR(..))
import Data.Show.Generic (genericShow)
import Common.RemoteConfig.Types as CRT

type TipsConfigRC = {
  sedan :: Array Int,
  suv :: Array Int,
  hatchback :: Array Int,
  autoRickshaw :: Array Int,
  taxi :: Array Int,
  taxiPlus :: Array Int,
  bike :: Array Int,
  suvPlus :: Array Int,
  heritageCab :: Array Int,
  default :: Array Int,
  ambulanceTaxi :: Array Int,
  ambulanceTaxiOxy :: Array Int,
  ambulanceAc :: Array Int,
  ambulanceAcOxy :: Array Int,
  ambulanceVentilator :: Array Int,
  bookAny :: Array Int
}

type SafetyVideoConfig
  = { videoId :: String
    , title :: String
    , coverImageUrl :: String
    , description :: Array DescriptionComponent
    }

newtype DescriptionComponent = DescriptionComponent {
  text :: String,
  color :: String,
  isBullet :: Boolean,
  marginLeft :: Int,
  marginTop :: Int,
  fontStyle :: String
}

derive instance genericDescriptionComponent :: Generic DescriptionComponent _
instance decodeDescriptionComponent :: Decode DescriptionComponent where decode = defaultDecode

type SpecialLocationsOb = {
  locations :: Array SpecialLocation
}

type SpecialLocation = {
  name :: String,
  gates :: Array Gate
}

type Gate = {
  gateName :: String,
  images :: Array PickupInstructions
}

type PickupInstructions = 
  { image :: String
  , title :: String
  }
  
newtype FamousDestination = FamousDestination {
  name :: String,
  address :: String,
  imageUrl :: String,
  lat :: Number,
  lon :: Number,
  description :: String,
  nameBasedOnLanguage :: String,
  dynamic_action :: Maybe CRT.RemoteAC
}

derive instance genericFamousDestination :: Generic FamousDestination _
instance decodeFamousDestination :: Decode FamousDestination where decode = defaultDecode

type Service = {
  type :: ServiceType,
  name :: STR,
  image :: String,
  backgroundColor :: String,
  preferredEstimateOrder :: Array String
}

data ServiceType = INSTANT | TRANSIT | INTERCITY | RENTAL | DELIVERY | INTERCITY_BUS | BIKE_TAXI | METRO | METRO_RIDE | METRO_OFFER | BUS | AMBULANCE_SERVICE

derive instance genericServiceType :: Generic ServiceType _
instance eqServiceType :: Eq ServiceType where eq = genericEq
instance showServiceType :: Show ServiceType where show = genericShow

type InterCityBusConfig = {
  baseUrl :: String
}

type SafetyConfig = {
  bannerAction :: String,
  bannerUrl :: String,
  bannerPosition :: Int,
  showOnRide :: String
}

type MetroConfig = {
  tnc :: String
}

type BusFlowConfig = {
  showBusTracking :: Boolean,
  showPostBookingTracking :: Boolean,
  liveRoutes :: Int,
  ticketValidity :: String,
  showPreBookingTracking :: Boolean
}

type CancellationThreshold = {
  showBanner :: Boolean,
  percentage :: Number
}