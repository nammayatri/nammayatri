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
import Common.Types.App

type TipsConfigRC = {
  sedan :: Array Int,
  suv :: Array Int,
  hatchback :: Array Int,
  autoRickshaw :: Array Int,
  taxi :: Array Int,
  taxiPlus :: Array Int,
  bike :: Array Int,
  suvPlus :: Array Int,
  default :: Array Int,
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
  preferredEstimateOrder :: Array String,
  hasSecondaryPill :: Boolean,
  secondaryPillColor :: String
}

data ServiceType = INSTANT | TRANSIT | INTERCITY | RENTAL | DELIVERY | INTERCITY_BUS | BIKE_TAXI | METRO | METRO_OFFER | BUS

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
  tnc :: String,
  logoImage :: String,
  mapImage :: String,
  bannerImage :: String,
  bannerBackgroundColor :: String,
  bannerTextColor :: String,
  showCancelButton :: Boolean
}

type BoostSearchConfig = {
  selectedEstimates :: Array String,
  selectedTip :: Int
}

type VariantBasedBoostSearchConfig = {
  sedan :: BoostSearchConfig,
  suv :: BoostSearchConfig,
  hatchback :: BoostSearchConfig,
  autoRickshaw :: BoostSearchConfig,
  taxi :: BoostSearchConfig,
  taxiPlus :: BoostSearchConfig,
  bike :: BoostSearchConfig,
  suvPlus :: BoostSearchConfig,
  default :: BoostSearchConfig,
  bookAny :: BoostSearchConfig
}

type RemoteCancellationReason = {
    cancellationReasons :: Array OptionButtonList
}

type EventsConfig = {
  enabled :: Boolean,
  pushEventChunkSize :: Int,
  loggingIntervalInMs :: Number
}

type BusFlowConfig = {
  showBusTracking :: Boolean,
  showPostBookingTracking :: Boolean,
  liveRoutes :: Int,
  ticketValidity :: String
}

type AppInfoConfig = {
  website :: String
}