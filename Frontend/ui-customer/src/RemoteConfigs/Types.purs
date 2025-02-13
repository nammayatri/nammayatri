module RemoteConfig.Types where

import Prelude
import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode)
import Presto.Core.Utils.Encoding (defaultDecode)
import Data.Maybe (Maybe)


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
  nameBasedOnLanguage :: String
}

derive instance genericFamousDestination :: Generic FamousDestination _
instance decodeFamousDestination :: Decode FamousDestination where decode = defaultDecode

-- type Service = {
--   type :: ServiceType,
--   name :: STR,
--   image :: String,
--   backgroundColor :: String,
--   preferredEstimateOrder :: Array String,
--   hasSecondaryPill :: Boolean,
--   secondaryPillColor :: String
-- }

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

type CancellationThreshold = {
  showBanner :: Boolean,
  percentage :: Number
}