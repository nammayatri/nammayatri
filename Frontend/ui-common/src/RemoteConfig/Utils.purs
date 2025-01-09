{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.RemoteConfig.Utils where

import Common.RemoteConfig.Types (RemoteConfig, RCCarousel(..), VariantLevelRemoteConfig(..), InvoiceConfig(..), ForwardBatchConfigData(..), TipsConfig, defaultForwardBatchConfigData, SubscriptionConfigVariantLevelEntity, SubscriptionConfigVariantLevel, GullakConfig, StuckRideFilterConfig, FeaturesConfigData(..), LottieSubscriptionInfo(..), LanguageKeyValue(..), defaultFeaturesConfigData, AppLanguage, AppConfigRC)
import DecodeUtil (decodeForeignObject, parseJSON, setAnyInWindow)
import Data.String (null, toLower)
import Data.Maybe (Maybe(..))
import Prelude (not, ($), (==), (||))
import Data.Maybe (fromMaybe)
import Data.Array (elem, filter, uncons)
import Data.Array as DA
import Data.Function.Uncurried (runFn3, runFn2)
import DecodeUtil (getAnyFromWindow)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import Common.RemoteConfig.Types as Types

foreign import fetchRemoteConfigString :: String -> String

foreign import fetchRemoteConfig :: forall a. String -> a

foreign import isWhiteListed :: String -> Array String -> Boolean

defaultCityRemoteConfig :: forall a. a -> RemoteConfig a
defaultCityRemoteConfig defaultValue =
  { bangalore : Just defaultValue
  , kolkata : Just defaultValue
  , chennai : Just defaultValue
  , tumakuru : Just defaultValue
  , mysore : Just defaultValue
  , paris : Just defaultValue
  , odisha : Just defaultValue
  , kochi : Just defaultValue
  , delhi : Just defaultValue
  , hyderabad : Just defaultValue
  , mumbai : Just defaultValue
  , coimbatore : Just defaultValue
  , pondicherry : Just defaultValue
  , goa : Just defaultValue
  , pune : Just defaultValue
  , tamilnaducities : Just defaultValue
  , default : defaultValue
  , noida : Just defaultValue
  , gurugram : Just defaultValue
  , vellore : Just defaultValue
  , hosur : Just defaultValue
  , madurai : Just defaultValue
  , thanjavur : Just defaultValue
  , tirunelveli : Just defaultValue
  , salem : Just defaultValue
  , trichy : Just defaultValue 
  , davanagere : Just defaultValue
  , shivamogga : Just defaultValue
  , hubli : Just defaultValue
  , mangalore : Just defaultValue
  , gulbarga : Just defaultValue
  , udupi : Just defaultValue
  , ysCities : Just defaultValue
  , bhubaneshwar : Just defaultValue
  , bhubaneswar : Just defaultValue
  , cuttack : Just defaultValue
  , nalgonda : Just defaultValue
  , puri : Just defaultValue
  , pudukkottai : Just defaultValue
  , bidar : Just defaultValue
  , config: Nothing
  }

carouselConfigData :: String -> String -> String -> String -> String -> String -> Array RCCarousel
carouselConfigData city configKey default userId categoryFilter variantFilter =
  let
    remoteConfig = fetchRemoteConfigString configKey

    parseVal = if not null remoteConfig then remoteConfig else fetchRemoteConfigString default

    decodedConfg = decodeForeignObject (parseJSON parseVal) $ defaultCityRemoteConfig []
  in
    filterWhiteListedConfigs userId $ filterCategoryBasedCarousel categoryFilter variantFilter $ getCityBasedConfig decodedConfg city

-- Each RCCarousel has a category field which is an array of strings, If the array is empty I want to include that RCCarousel in output array, but if it has some values I want to match check `elem` if the categoryFilter is present in the array or not. If it is present then include that RCCarousel in the output array.
filterCategoryBasedCarousel :: String -> String -> Array RCCarousel -> Array RCCarousel
filterCategoryBasedCarousel allowedFilter variantFilter configs =
  let
    filteredConfigs = filter (\x -> validateConfig x) configs
  in
    filteredConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      categoryList = fromMaybe [] config.categoryFilter
    in
      if DA.null categoryList then true else elem allowedFilter categoryList || elem variantFilter categoryList

fetchWhiteListedUser :: String -> Array String
fetchWhiteListedUser configKey = fetchRemoteConfig configKey

filterWhiteListedConfigs :: String -> Array RCCarousel -> Array RCCarousel
filterWhiteListedConfigs userId configs =
  let
    whiteListedConfigs = filter (\x -> validateConfig x) configs
  in
    whiteListedConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      whiteListedUserListArray = fromMaybe [] config.whitelist
    in
      if DA.null whiteListedUserListArray then true else validateUser whiteListedUserListArray

  validateUser :: Array String -> Boolean
  validateUser parameterList = case uncons parameterList of
    Just { head: x, tail: xs } ->
      let
        userList = fetchWhiteListedUser x
      in
        if isWhiteListed userId userList then true else validateUser xs -- TODO:: Need to check why it's not working within PS and replace with Map for optimisation
    Nothing -> false

forwardBatchConfigData :: String -> ForwardBatchConfigData
forwardBatchConfigData city =
  let
    remoteConfig = fetchRemoteConfigString "Forward_Dispatch_Feature"
    decodedConfg = decodeForeignObject (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultForwardBatchConfigData
  in 
    getCityBasedConfig decodedConfg $ toLower city

featuresConfigData :: String -> FeaturesConfigData
featuresConfigData city =
  let
    remoteConfig = fetchRemoteConfigString "feature_configs"
    decodedConfig = decodeForeignObject (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultFeaturesConfigData
  in
    getCityBasedConfig decodedConfig $ toLower city

getAppBasedConfig :: forall a. AppConfigRC a -> String -> a
getAppBasedConfig config app = case app of
  "Namma Yatri Partner" -> fromMaybe config.default config.nammaYatriPartner
  "Odisha Yatri Partner" -> fromMaybe config.default config.odishaYatriPartner
  "Kerala Savaari Partner" -> fromMaybe config.default config.keralaSavaariPartner
  "Yatri Driver" -> fromMaybe config.default config.yatriPartner
  "Namma Yatri" -> fromMaybe config.default config.nammaYatri
  "Odisha Yatri" -> fromMaybe config.default config.odishaYatri
  "Yatri" -> fromMaybe config.default config.yatri
  "Yatri Sathi" -> fromMaybe config.default config.yatriSathi
  _ -> config.default

getCityBasedConfig :: forall a. RemoteConfig a -> String -> a
getCityBasedConfig config city = case city of
  "bangalore" -> fromMaybe config.default config.bangalore
  "paris" -> fromMaybe config.default config.paris
  "odisha" -> fromMaybe config.default config.odisha
  "kolkata" -> fromMaybe config.default config.kolkata
  "chennai" -> fromMaybe config.default config.chennai
  "mysore" -> fromMaybe config.default config.mysore
  "tumakuru" -> fromMaybe config.default config.tumakuru
  "kochi" -> fromMaybe config.default config.kochi
  "delhi" -> fromMaybe config.default config.delhi
  "hyderabad" -> fromMaybe config.default config.hyderabad
  "mumbai" -> fromMaybe config.default config.mumbai
  "coimbatore" -> fromMaybe config.default config.coimbatore
  "pondicherry" -> fromMaybe config.default config.pondicherry
  "goa" -> fromMaybe config.default config.goa
  "pune" -> fromMaybe config.default config.pune
  "tamilnaducities" -> fromMaybe config.default config.tamilnaducities
  "noida" -> fromMaybe config.default config.noida
  "gurugram" -> fromMaybe config.default config.gurugram
  "vellore" -> fromMaybe config.default config.vellore
  "hosur" -> fromMaybe config.default config.hosur
  "madurai" -> fromMaybe config.default config.madurai
  "thanjavur" -> fromMaybe config.default config.thanjavur
  "tirunelveli" -> fromMaybe config.default config.tirunelveli
  "salem" -> fromMaybe config.default config.salem
  "trichy" -> fromMaybe config.default config.trichy
  "davanagere" -> fromMaybe config.default config.davanagere
  "shivamogga" -> fromMaybe config.default config.shivamogga
  "hubli" -> fromMaybe config.default config.hubli
  "mangalore" -> fromMaybe config.default config.mangalore
  "gulbarga" -> fromMaybe config.default config.gulbarga
  "udupi" -> fromMaybe config.default config.udupi
  "bhubaneshwar" -> fromMaybe config.default config.bhubaneswar
  "bhubaneswar" -> fromMaybe config.default config.bhubaneswar
  "cuttack" -> fromMaybe config.default config.cuttack
  "nalgonda" -> fromMaybe config.default config.nalgonda
  "puri" -> fromMaybe config.default config.puri
  "pudukkottai" -> fromMaybe config.default config.pudukkottai
  "bidar" -> fromMaybe config.default config.bidar
  _ -> case (getMerchant FunctionCall) of
        YATRISATHI -> fromMaybe config.default config.ysCities
        _ -> config.default

tipConfigData :: String -> String -> Array Int
tipConfigData city variant = do
  let
    tipsConfig = runFn3 getAnyFromWindow "tips_config" Nothing Just
    decodedConfig = case tipsConfig of
          Just (config :: (RemoteConfig TipsConfig)) -> config
          Nothing -> do
            let remoteConfig = fetchRemoteConfigString "tips_config"
                decodedConfg = decodeForeignObject (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultTipsConfig
                _ = runFn2 setAnyInWindow "tips_config" decodedConfg
            decodedConfg
  getTipForVariant variant $ getCityBasedConfig decodedConfig $ toLower city
  where
    -- if a variant tip is not provided for a particular city we will check for default variant config for that city. If default is not there then tip wont be there.
    getTipForVariant variant config = case getTip config variant of
      Nothing -> fromMaybe [] $ getTip config "default"
      Just tips -> tips

    getTip config variant = 
      case variant of 
        "SEDAN" -> config.sedan
        "SUV" -> config.suv
        "HATCHBACK" -> config.hatchback
        "AUTO_RICKSHAW" -> config.autoRickshaw
        "EV_AUTO_RICKSHAW" -> config.evAutoRickshaw
        "TAXI" -> config.taxi
        "TAXI_PLUS" -> config.taxiPlus
        "BIKE" -> config.bike
        "BOOK_ANY" -> config.bookAny
        "SUV_PLUS" -> config.suv
        "DELIVERY_BIKE" -> config.deliveryBike
        "AMBULANCE_AC" -> config.ambulanceAc
        "AMBULANCE_AC_OXY" -> config.ambulanceAcOxy
        "AMBULANCE_VENTILATOR" -> config.ambulanceVentilator
        "AMBULANCE_TAXI" -> config.ambulanceTaxi
        "AMBULANCE_TAXI_OXY" -> config.ambulanceTaxiOxy
        "HERITAGE_CAB" -> config.heritageCab
        _ -> config.default

defaultTipsConfig :: TipsConfig
defaultTipsConfig = 
  { sedan: Nothing
  , suv: Nothing
  , hatchback: Nothing
  , autoRickshaw: Nothing
  , taxi: Nothing
  , taxiPlus: Nothing
  , bike : Nothing
  , bookAny: Nothing
  , deliveryBike: Nothing
  , ambulanceTaxi : Nothing
  , ambulanceTaxiOxy : Nothing
  , ambulanceAc : Nothing
  , ambulanceAcOxy : Nothing
  , ambulanceVentilator : Nothing
  , evAutoRickshaw: Nothing
  , heritageCab: Nothing
  , default: Nothing
  }


defaultSubscriptionsConfigVariantLevel :: SubscriptionConfigVariantLevel
defaultSubscriptionsConfigVariantLevel = 
  { sedan: Nothing
  , suv: Nothing
  , hatchback: Nothing
  , autoRickshaw: Nothing
  , taxi: Nothing
  , taxiPlus: Nothing
  , bookAny: Nothing
  , deliveryBike: Nothing
  , ambulanceTaxi : Nothing
  , ambulanceTaxiOxy : Nothing
  , ambulanceAc : Nothing
  , ambulanceAcOxy : Nothing
  , ambulanceVentilator : Nothing
  , evAutoRickshaw: Nothing
  , heritageCab: Nothing
  , default: Nothing
  }


defaultLottieSubscriptionInfo :: LottieSubscriptionInfo
defaultLottieSubscriptionInfo = {
  freeTrialLottie : defaultFreeTrialLottie,
  introductoryLottie : defaultIntroductoryLottie,
  subscriptionPlanLottie: defaultSubscriptionPlanLottie
}

defaultFreeTrialLottie :: LanguageKeyValue
defaultFreeTrialLottie = {
  english: "lottie/ny_ic_subscription_info_01.json",
  hindi: "lottie/ny_ic_subscription_info_hindi_01.json",
  kannada: "lottie/ny_ic_subscription_info_kannada_01.json",
  tamil: "lottie/ny_ic_subscription_info_tamil_01.json",
  bengali: "lottie/ny_ic_subscription_info_bengali_01.json",
  telugu: "lottie/ny_ic_subscription_info_01.json",
  malayalam: "lottie/ny_ic_subscription_info_malayalam_01.json",
  default: "lottie/ny_ic_subscription_info_01.json"
}

defaultIntroductoryLottie :: LanguageKeyValue
defaultIntroductoryLottie = {
  english: "lottie/ny_sub_intro_english_apr1.json",
  hindi: "lottie/ny_sub_intro_hindi_apr1.json",
  kannada: "lottie/ny_sub_intro_kannada_apr1.json",
  tamil: "lottie/ny_sub_intro_tamil_apr1.json",
  bengali: "lottie/ny_ic_subscription_info_bengali_03_2.json",
  telugu: "lottie/ny_sub_intro_telugu_apr1.json",
  malayalam: "lottie/ny_sub_intro_malayalam_apr1.json",
  default: "lottie/ny_sub_intro_english_apr1.json"
}

defaultSubscriptionPlanLottie :: LanguageKeyValue
defaultSubscriptionPlanLottie = {
  english: "lottie/ny_ic_subscription_info_02.json",
  hindi: "lottie/ny_ic_subscription_info_hindi_02.json",
  kannada: "lottie/ny_ic_subscription_info_kannada_02.json",
  tamil: "lottie/ny_ic_subscription_info_tamil_02.json",
  bengali: "lottie/ny_ic_subscription_info_bengali_02.json",
  telugu: "lottie/ny_ic_subscription_info_02.json",
  malayalam: "lottie/ny_ic_subscription_info_malayalam_02.json",
  default: "lottie/ny_ic_subscription_info_02.json"
}

defaultSubscriptionsConfigVariantLevelEntity :: SubscriptionConfigVariantLevelEntity
defaultSubscriptionsConfigVariantLevelEntity = 
  { 
    noChargesTillDate : "Oct 1st 2024-*$*-ಅಕ್ಟೋಬರ್ 01, ರವರೆಗೆ-*$*-1 अक्टूबर 2024-*$*-১লা অক্টোবর, ২০২৪-*$*-ഒക്ടോബര്‍ 1, 2024-*$*-1 அக்டோபர் 2024-*$*-1 అక్టోబర్ 2024",
    lowestFeesFromDate : "Oct 2nd 2024-*$*-ಅಕ್ಟೋಬರ್ 2, 2024-*$*-2 अक्टूबर 2024-*$*-২য় অক্টোবর, ২০২৪-*$*-രണ്ടാം ഒക്ടോബര്‍, 2024-*$*-அக்டோபர் 2, 2024-*$*-అక్టోబరు 2, 2024",
    useFreeTrialLottie : Just false,
    earnUptoAmout : Just 5000,
    yatriPlansPlaylist : Just "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK",
    enableSubscriptionSupportPopup : Just false,
    offerBannerConfig : Just defaultOfferBannerConfig,
    enableSubsV2 : Just false,
    duesConfig : Just defSubscriptionDues,
    freeTrialPopupDaysList : Just [3,2,1],
    freeTrialPopupOnRidesList : Just [5,2],
    lottieSubscriptionInfo : Just defaultLottieSubscriptionInfo
  }

subscriptionsConfigVariantLevel :: String -> String -> SubscriptionConfigVariantLevelEntity
subscriptionsConfigVariantLevel city variant = do
  let
    subscriptionConfig = runFn3 getAnyFromWindow "subscription_config_variant_level" Nothing Just
    decodedConfig = case subscriptionConfig of
          Just (config :: (RemoteConfig SubscriptionConfigVariantLevel)) -> config
          Nothing -> do
            let remoteConfig = fetchRemoteConfigString "subscription_config_variant_level"
                decodedConfg = decodeForeignObject (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultSubscriptionsConfigVariantLevel
                _ = runFn2 setAnyInWindow "subscription_config_variant_level" decodedConfg
            decodedConfg
  getConfigForVariant variant $ getCityBasedConfig decodedConfig $ toLower city
  where
    getConfigForVariant variant config = case getSubscriptionConfig config variant of
      Nothing -> fromMaybe defaultSubscriptionsConfigVariantLevelEntity $ getSubscriptionConfig config "default"
      Just config -> config

    getSubscriptionConfig config variant = 
      case variant of 
        "SEDAN" -> config.sedan
        "SUV" -> config.suv
        "HATCHBACK" -> config.hatchback
        "AUTO_RICKSHAW" -> config.autoRickshaw
        "EV_AUTO_RICKSHAW" -> config.evAutoRickshaw
        "TAXI" -> config.taxi
        "TAXI_PLUS" -> config.taxiPlus
        "BOOK_ANY" -> config.bookAny
        "DELIVERY_BIKE" -> config.deliveryBike
        "AMBULANCE_AC" -> config.ambulanceAc
        "AMBULANCE_AC_OXY" -> config.ambulanceAcOxy
        "AMBULANCE_VENTILATOR" -> config.ambulanceVentilator
        "AMBULANCE_TAXI" -> config.ambulanceTaxi
        "AMBULANCE_TAXI_OXY" -> config.ambulanceTaxiOxy
        "HERITAGE_CAB" -> config.heritageCab
        _ -> config.default

defaultGullakConfig :: GullakConfig
defaultGullakConfig = 
  { image: "",
    enabled : false,
    videoUrl : Just "https://youtu.be/JRhT26ib2L0?si=Wtkmm-V2Kfj-Qr3k"
  }
  
defaultStuckRideFilterConfig :: StuckRideFilterConfig
defaultStuckRideFilterConfig = 
  { estimatedDurationFallback : 20,
    buffer : 360000.0,
    enable : false
  }

defaultAppRemoteConfig :: forall a. a -> AppConfigRC a
defaultAppRemoteConfig defaultValue =
  { nammaYatri: Just defaultValue
  , nammaYatriPartner: Just defaultValue
  , odishaYatri: Just defaultValue
  , odishaYatriPartner: Just defaultValue
  , keralaSavaariPartner: Just defaultValue
  , yatri: Just defaultValue
  , yatriPartner: Just defaultValue
  , manaYatri: Just defaultValue
  , manaYatriPartner: Just defaultValue
  , yatriSathi: Just defaultValue
  , yatriSathiPartner: Just defaultValue
  , default: defaultValue 
  }

defaultLanguageConfig :: Array AppLanguage
defaultLanguageConfig = 
  [
    { name:"English",
      value:"EN_US",
      subtitle: "ಆಂಗ್ಲ"
    },
    { name:"ಕನ್ನಡ",
      value:"KN_IN",
      subtitle: "Kannada"
    },
    {
      name: "ଓଡିଆ",
      value: "OD_IN",
      subtitle: "Odiya"
    },
    { name:"हिंदी",
      value:"HI_IN",
      subtitle: "Hindi"
    },
    { name:"தமிழ்",
      value:"TA_IN",
      subtitle: "Tamil"
    },
    { name:"తెలుగు",
      value:"TE_IN",
      subtitle: "Telugu"
    }
  ]

stuckRideFilterConfig :: String -> StuckRideFilterConfig
stuckRideFilterConfig _ =
  let config = fetchRemoteConfigString "stuck_ride_filter"
  in decodeForeignObject (parseJSON config) defaultStuckRideFilterConfig

gullakConfig :: String -> GullakConfig
gullakConfig city = do
    let config = fetchRemoteConfigString "gullak_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultGullakConfig
    getCityBasedConfig value $ toLower city

appLanguageConfig :: String -> Array AppLanguage
appLanguageConfig appName = do
  let config = fetchRemoteConfigString "enabled_app_languages"
      value = decodeForeignObject (parseJSON config) $ defaultAppRemoteConfig defaultLanguageConfig
  getAppBasedConfig value appName

defaultOfferBannerConfig :: Types.OfferBanner
defaultOfferBannerConfig = {
    showOfferBanner : false,
    offerBannerValidTill : "2025-01-01T00:00:00",
    offerBannerDeadline : "January 1-*$*-ಜನವರಿ 1-*$*-1 जनवरी-*$*-ஜனவரி 1",
    offerBannerPlans : ["a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d"],
    payAmount : "10"
  }

defSubscriptionDues :: Types.RCSubscriptionDues
defSubscriptionDues = {
    max_dues_limit : 100.0,
    low_dues_warning_limit : 25.0,
    high_due_warning_limit : 75.0
}

getConfigForVariant :: forall a. String -> Types.VariantLevelRemoteConfig a -> a
getConfigForVariant variant config = 
  case variant of 
    "SEDAN" -> config.sedan
    "SUV" -> config.suv
    "HATCHBACK" -> config.hatchback
    "AUTO_RICKSHAW" -> config.autoRickshaw
    "EV_AUTO_RICKSHAW" -> config.evAutoRickshaw
    "TAXI" -> config.taxi
    "TAXI_PLUS" -> config.taxiPlus
    "BOOK_ANY" -> config.bookAny
    "DELIVERY_BIKE" -> config.deliveryBike
    "AMBULANCE_AC" -> config.ambulanceAc
    "AMBULANCE_AC_OXY" -> config.ambulanceAcOxy
    "AMBULANCE_VENTILATOR" -> config.ambulanceVentilator
    "AMBULANCE_TAXI" -> config.ambulanceTaxi
    "AMBULANCE_TAXI_OXY" -> config.ambulanceTaxiOxy
    "HERITAGE_CAB" -> config.heritageCab
    _ -> config.default
      
getInvoiceConfig :: String -> String -> InvoiceConfig
getInvoiceConfig variant city =
  let remoteConfig = fetchRemoteConfigString "show_invoice_text_config"
      decodedConfig = decodeForeignObject (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultInvoiceVariantConfig
  in
  getVariantLevelConfig variant $ getCityBasedConfig decodedConfig $ toLower city
  where
    getVariantLevelConfig variant config = case getConfigForVariant variant config of
       Nothing -> {isEnabled : Just true}
       Just variantConfig -> variantConfig

defaultInvoiceVariantConfig :: Types.DriverInvoiceConfigVariantLevel
defaultInvoiceVariantConfig = 
  { sedan: Nothing
  , suv: Nothing
  , hatchback: Nothing
  , autoRickshaw: Nothing
  , taxi: Nothing
  , taxiPlus: Nothing
  , bookAny: Nothing
  , deliveryBike: Nothing
  , ambulanceTaxi : Nothing
  , ambulanceTaxiOxy : Nothing
  , ambulanceAc : Nothing
  , ambulanceAcOxy : Nothing
  , ambulanceVentilator : Nothing
  , evAutoRickshaw: Nothing
  , heritageCab: Nothing
  , default: Nothing
  }

defaultVoipConfig :: Types.VoipConfig
defaultVoipConfig = {
  customer : {
    enableVoipFeature : false,
    enableVoipCalling : false
  },
  driver : {
    enableVoipFeature : false,
    enableVoipCalling : false
  }
}