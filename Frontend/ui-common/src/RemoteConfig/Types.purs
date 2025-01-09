{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.RemoteConfig.Types where

import Prelude
import Data.Maybe (Maybe)
import Foreign.Class  (class Decode, decode, class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding  (defaultDecode, defaultDecode, defaultEncode)
import Control.Monad.Except (runExcept, except)
import Foreign.Index (readProp)
import Data.Either as Either
import Data.Eq.Generic (genericEq)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

type RemoteConfig a
  = { bangalore :: Maybe a
    , kolkata :: Maybe a
    , chennai :: Maybe a
    , tumakuru :: Maybe a
    , paris :: Maybe a
    , odisha :: Maybe a
    , mysore :: Maybe a
    , kochi :: Maybe a
    , delhi :: Maybe a
    , hyderabad :: Maybe a
    , mumbai :: Maybe a
    , coimbatore :: Maybe a
    , pondicherry :: Maybe a
    , goa :: Maybe a
    , pune :: Maybe a
    , tamilnaducities :: Maybe a
    , default :: a
    , noida :: Maybe a
    , gurugram :: Maybe a
    , vellore :: Maybe a
    , hosur :: Maybe a
    , madurai :: Maybe a
    , thanjavur :: Maybe a
    , tirunelveli :: Maybe a
    , salem :: Maybe a
    , trichy :: Maybe a 
    , davanagere :: Maybe a
    , shivamogga :: Maybe a
    , hubli :: Maybe a
    , mangalore :: Maybe a
    , gulbarga :: Maybe a
    , udupi :: Maybe a
    , ysCities :: Maybe a
    , bhubaneshwar :: Maybe a
    , bhubaneswar :: Maybe a
    , cuttack :: Maybe a
    , nalgonda :: Maybe a
    , bidar :: Maybe a
    , puri :: Maybe a
    , pudukkottai :: Maybe a
    , config :: Maybe Config
    }
    
type AppConfigRC a =
  { nammaYatri :: Maybe a
  , nammaYatriPartner :: Maybe a
  , odishaYatri :: Maybe a
  , odishaYatriPartner :: Maybe a
  , keralaSavaariPartner :: Maybe a
  , yatri :: Maybe a
  , yatriPartner :: Maybe a
  , manaYatri :: Maybe a
  , manaYatriPartner :: Maybe a
  , yatriSathi :: Maybe a
  , yatriSathiPartner :: Maybe a
  , default :: a
  }

type VariantLevelRemoteConfig a = 
    { autoRickshaw :: a,
      suv :: a,
      sedan :: a,
      hatchback :: a,
      bookAny :: a,
      taxi :: a,
      taxiPlus :: a,
      ambulanceTaxi :: a,
      ambulanceTaxiOxy ::  a,
      ambulanceAc ::  a,
      ambulanceAcOxy ::  a,
      ambulanceVentilator :: a,
      default :: a,
      deliveryBike :: a,
      evAutoRickshaw :: a,
      heritageCab :: a
    }


newtype RCCarousel
  = RCCarousel
  { text_color :: String
  , text :: String
  , cta_text :: String
  , cta_action :: Maybe String
  , cta_link :: String
  , cta_icon :: String
  , banner_color :: String
  , banner_image :: String
  , cta_background_color :: String
  , cta_text_color :: String
  , cta_corner_radius :: String
  , cta_image_url :: String
  , whitelist :: Maybe (Array String)
  , categoryFilter :: Maybe (Array String)
  , image_banner :: Maybe String
  , dynamic_action :: Maybe RemoteAC
  , accessibilityHint :: Maybe String
  , showDuringRide :: Maybe Boolean
  }

derive instance genericRCCarousel :: Generic RCCarousel _

instance decodeRCCarousel :: Decode RCCarousel where
  decode = defaultDecode

instance encodeRCCarousel :: Encode RCCarousel where
  encode = defaultEncode

newtype Config
  = Config
  { randomOrder :: Boolean
  }

derive instance genericConfig :: Generic Config _

instance decodeConfig :: Decode Config where
  decode = defaultDecode

instance encodeConfig :: Encode Config where
  encode = defaultEncode


newtype ForwardBatchConfigData = ForwardBatchConfigData
  { is_Forward_Dispatch_Feature_Enabled :: Boolean,
    advancedRidePopUpYoutubeLink :: String,
    callDriverInfoPost :: Boolean
  }
derive instance genericForwardBatchConfigData :: Generic ForwardBatchConfigData _

instance decodeForwardBatchConfigData :: Decode ForwardBatchConfigData where
  decode = defaultDecode

instance encodeForwardBatchConfigData :: Encode ForwardBatchConfigData where
  encode = defaultEncode

defaultForwardBatchConfigData :: ForwardBatchConfigData
defaultForwardBatchConfigData = ForwardBatchConfigData
  { is_Forward_Dispatch_Feature_Enabled: false,
    advancedRidePopUpYoutubeLink: "",
    callDriverInfoPost: false
  }

newtype FeaturesConfigData = FeaturesConfigData
  { enableDeliveryBike :: Boolean
  }
derive instance genericFeaturesConfigData :: Generic FeaturesConfigData _

instance decodeFeaturesConfigData :: Decode FeaturesConfigData where
  decode = defaultDecode

instance encodeFeaturesConfigData :: Encode FeaturesConfigData where
  encode = defaultEncode

defaultFeaturesConfigData :: FeaturesConfigData
defaultFeaturesConfigData = FeaturesConfigData
  { enableDeliveryBike: false
  }

type TipsConfig
  = { autoRickshaw :: Maybe (Array Int),
      suv :: Maybe (Array Int),
      sedan :: Maybe (Array Int),
      hatchback :: Maybe (Array Int),
      bookAny :: Maybe (Array Int),
      taxi :: Maybe (Array Int),
      taxiPlus :: Maybe (Array Int),
      bike :: Maybe (Array Int),
      default :: Maybe (Array Int),
      deliveryBike :: Maybe (Array Int),
      evAutoRickshaw :: Maybe (Array Int),
      ambulanceTaxi :: Maybe (Array Int),
      ambulanceTaxiOxy ::  Maybe (Array Int),
      ambulanceAc ::  Maybe (Array Int),
      ambulanceAcOxy ::  Maybe (Array Int),
      ambulanceVentilator :: Maybe (Array Int),
      heritageCab :: Maybe (Array Int)
    }

type SubscriptionConfigVariantLevel 
  = VariantLevelRemoteConfig (Maybe SubscriptionConfigVariantLevelEntity)

type AppLanguage
  = {
    name :: String,
    value :: String,
    subtitle :: String
  }

type SubscriptionConfigVariantLevelEntity = {
   noChargesTillDate :: String,
   lowestFeesFromDate :: String,
   useFreeTrialLottie :: Maybe Boolean,
   earnUptoAmout :: Maybe Int,
   yatriPlansPlaylist :: Maybe String,
   enableSubscriptionSupportPopup :: Maybe Boolean,
   offerBannerConfig :: Maybe OfferBanner,
   enableSubsV2 :: Maybe Boolean,
   duesConfig :: Maybe RCSubscriptionDues,
   freeTrialPopupDaysList :: Maybe (Array Int),
   freeTrialPopupOnRidesList :: Maybe (Array Int),
   lottieSubscriptionInfo :: Maybe LottieSubscriptionInfo
}

type LottieSubscriptionInfo = {
  freeTrialLottie :: LanguageKeyValue,
  introductoryLottie :: LanguageKeyValue,
  subscriptionPlanLottie :: LanguageKeyValue
}

type LanguageKeyValue = {
  english :: String,
  hindi :: String,
  kannada :: String,
  tamil :: String,
  bengali :: String,
  telugu :: String,
  malayalam :: String,
  default :: String
}

type OfferBanner = {
    showOfferBanner :: Boolean,
    offerBannerValidTill :: String,
    offerBannerDeadline :: String,
    offerBannerPlans :: Array String,
    payAmount :: String
}

type RCSubscriptionDues = {
    max_dues_limit :: Number,
    low_dues_warning_limit :: Number,
    high_due_warning_limit :: Number
}
  
---------------------------------Remote Config Dynamic AC-----------------------------------------------

data RemoteAC = Destination DestinationParams | WhereTo | Profile | MetroBooking | WebLink WebLinkParams | UpdateProfile | NoAction | Safety | ZooBooking | Rentals | Intercity | SafetyExplaination | SetupSafety | IntercityBus | AmbulanceBooking

instance eqRemoteAC :: Eq RemoteAC where eq = genericEq
instance encodeJsonRemoteAC :: EncodeJson RemoteAC where encodeJson = genericEncodeJson
instance decodeJsonRemoteAC :: DecodeJson RemoteAC where decodeJson = genericDecodeJson

derive instance genericRemoteAC :: Generic RemoteAC _
instance decodeRemoteAC :: Decode RemoteAC where 
  decode body = 
    let default = runExcept $ defaultDecode body
    in except $ if Either.isRight default then default else Either.Right $ NoAction
instance encodeRemoteAC :: Encode RemoteAC where encode = defaultEncode

newtype DestinationParams = DestinationParams {
  lat :: Number,
  lng :: Number,
  description :: Maybe String,
  fullAddress :: Maybe String,
  placeId :: Maybe String
}

derive instance genericDestinationParams :: Generic DestinationParams _
instance decodeDestinationParams :: Decode DestinationParams where decode = defaultDecode
instance encodeDestinationParams :: Encode DestinationParams where encode = defaultEncode
instance eqDestinationParams :: Eq DestinationParams where eq = genericEq
instance encodeJsonDestinationParams:: EncodeJson DestinationParams where encodeJson = genericEncodeJson
instance decodeJsonDestinationParams :: DecodeJson DestinationParams where decodeJson = genericDecodeJson

newtype WebLinkParams = WebLinkParams {
  url :: String
}

derive instance genericWebLinkParams :: Generic WebLinkParams _
instance decodeWebLinkParams :: Decode WebLinkParams where decode = defaultDecode
instance encodeWebLinkParams :: Encode WebLinkParams where encode = defaultEncode
instance eqWebLinkParams :: Eq WebLinkParams where eq = genericEq
instance encodeJsonWebLinkParams:: EncodeJson WebLinkParams where encodeJson = genericEncodeJson
instance decodeJsonWebLinkParams :: DecodeJson WebLinkParams where decodeJson = genericDecodeJson

type GullakConfig = {
  image :: String,
  enabled :: Boolean,
  videoUrl :: Maybe String
}
type StuckRideFilterConfig = {
  estimatedDurationFallback :: Int,
  buffer :: Number,
  enable :: Boolean
}

type BundleLottieConfig = {
  lottieUrl :: String
, enable :: Boolean
}

type InvoiceConfig = {
  isEnabled :: Maybe Boolean
}

type DriverInvoiceConfigVariantLevel = VariantLevelRemoteConfig (Maybe InvoiceConfig)

type VoipConfig = {
  customer :: {
    enableVoipFeature :: Boolean,
    enableVoipCalling :: Boolean
  },
  driver :: {
    enableVoipFeature :: Boolean,
    enableVoipCalling :: Boolean
  }
}
