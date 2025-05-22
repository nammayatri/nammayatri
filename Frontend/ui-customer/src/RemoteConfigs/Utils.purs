module RemoteConfig.Utils where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON , decodeForeignAny)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Common.RemoteConfig (fetchRemoteConfigString, getCityBasedConfig, getAppBasedConfig, defaultCityRemoteConfig, defaultAppRemoteConfig, BundleLottieConfig, RemoteAC(..))
import Data.Maybe (Maybe(..), maybe)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)
import Data.Function (on)
import Data.String as DS
import Common.Types.App
import RemoteConfig.Types
import Data.Array as DA
import MerchantConfig.Types (Language)
import Locale.Utils(getLanguageLocale)
import Constants (languageKey)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Show(show)
import Debug (spy)
import LocalStorage.Cache
import Data.Function.Uncurried
import RemoteConfig.Types as Types

safetyVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig []
    getCityBasedConfig value city

safetyBannerVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyBannerVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_banner_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig []
    getCityBasedConfig value city

pickupInstructions :: String -> String -> String -> Array PickupInstructions
pickupInstructions placeName gateName language = do
    let config = fetchRemoteConfigString ("pickup_instructions_" <> language)
        pickupPlaces = decodeForeignObject (parseJSON config) defPlaces
        compareStrings = on (==) DS.trim
        locationsArr = pickupPlaces.locations
        myMbSpecialLocation = DA.find (\el -> compareStrings el.name placeName) locationsArr
    case myMbSpecialLocation of
        Just specialLocation -> do
            let gates = specialLocation.gates
                myMbgate = DA.find (\el -> compareStrings el.gateName gateName) gates
            case myMbgate of
                Just gate -> gate.images
                Nothing -> []
        Nothing -> []

defPlaces :: SpecialLocationsOb
defPlaces = {
    locations : []
}

getFamousDestinations :: String -> Array FamousDestination
getFamousDestinations city = do
    let langConfig = fetchRemoteConfigString $ "famous_destinations" <> (getLanguage $ getLanguageLocale languageKey)
        config = if not $ DS.null langConfig
                   then langConfig
                   else fetchRemoteConfigString "famous_destinations_en"
        value = decodeConfig config
        famousDestinationsBasedOnLanguage = getCityBasedConfig value city
    case famousDestinationsBasedOnLanguage of
        [] -> getCityBasedConfig defaultConfigInEnglish city
        _ -> famousDestinationsBasedOnLanguage
  where
    getLanguage lang =
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"
    decodeConfig config = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig []
    defaultConfigInEnglish = decodeConfig (fetchRemoteConfigString "famous_destinations_en")

getEstimatesOrder :: String -> Array String
getEstimatesOrder city = do
    let config = fetchRemoteConfigString "estimates_order"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ["AUTO_RICKSHAW", "BOOK_ANY"]
    getCityBasedConfig value city

getEstimatesOrderBaseOnServiceTier :: String -> Array String
getEstimatesOrderBaseOnServiceTier city = do
    let config = fetchRemoteConfigString "estimates_order_service_tier"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ["Auto", "Book Any"]
    getCityBasedConfig value city


getPreferredVariant :: String -> String
getPreferredVariant city = do
    let config = fetchRemoteConfigString "preferred_estimate_order"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ""
    getCityBasedConfig value city

getBookAnyServices :: String -> Array String
getBookAnyServices city = do
    let config = fetchRemoteConfigString "book_any_services"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ["Auto", "Non-AC Mini", "AC Mini", "Sedan", "XL Cab"]
    getCityBasedConfig value city

getBookAnySelectedServices :: String -> Array String
getBookAnySelectedServices city = do
    let config = fetchRemoteConfigString "book_any_selected_services"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ["Auto", "Non-AC Mini", "AC Mini"]
    getCityBasedConfig value city

customerAppLanguageConfig :: String -> Array Language
customerAppLanguageConfig appName = do
  let config = fetchRemoteConfigString "enabled_app_languages"
      value = decodeForeignObject (parseJSON config) $ defaultAppRemoteConfig []
  getAppBasedConfig value appName

getLocationSuggestionsToExclude :: String -> Array String
getLocationSuggestionsToExclude city = do
    let config = fetchRemoteConfigString "location_suggestions_to_exclude"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig []
    getCityBasedConfig value city

getEnabledServices :: String -> Array String
getEnabledServices city = do
    let config = fetchRemoteConfigString "enabled_services"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig []
    getCityBasedConfig value city

getPreferredOrderInBookAny :: String -> Boolean
getPreferredOrderInBookAny city = do
    let config = fetchRemoteConfigString "preferred_book_any_options"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig false
    getCityBasedConfig value city

getBusFlowConfigs :: String -> BusFlowConfig
getBusFlowConfigs city = do
    let config = fetchRemoteConfigString "bus_flow_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig {showBusTracking : false, showPostBookingTracking : false, liveRoutes : 0, ticketValidity : ""}
    getCityBasedConfig value $ DS.toLower city

defaultTipConfig :: TipsConfigRC
defaultTipConfig = {
  sedan : [0, 20, 30, 50],
  suv : [0, 20, 30, 50],
  hatchback : [0, 20, 30, 50],
  autoRickshaw : [0, 10, 20, 30],
  taxi : [0, 20, 30, 50],
  taxiPlus : [0, 20, 30, 50],
  bike : [0, 20, 30, 50],
  suvPlus : [0, 20, 30, 50],
  default : [0, 10, 20, 30],
  bookAny : [0, 10, 20, 30]
}

getTipConfigRC :: String -> TipsConfigRC
getTipConfigRC city = do
    let config = fetchRemoteConfigString "tip_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultTipConfig
    getCityBasedConfig value city


getInterCityBusConfig :: String -> InterCityBusConfig
getInterCityBusConfig lazy = decodeForeignObject (parseJSON $ fetchRemoteConfigString "intercity_bus_config") $ {baseUrl : "https://app-nammayatri.redbus.in/"}

getBundleSplashConfig :: String -> BundleLottieConfig
getBundleSplashConfig lazy = decodeForeignObject (parseJSON $ fetchRemoteConfigString "customer_bundle_splash_config") $ { lottieUrl : "https://assets.moving.tech/beckn/nammayatri/user/lottie/ny_bundle_splash_lottie_new.json", enable : true}

getSafetyConfig :: LazyCheck -> SafetyConfig
getSafetyConfig _ =
    let config = fetchRemoteConfigString "safety_configs"
        value = decodeForeignObject (parseJSON config) defaultSafetyConfig
    in value

defaultSafetyConfig :: SafetyConfig
defaultSafetyConfig = {
    bannerAction : "",
    bannerUrl : "",
    bannerPosition : 0,
    showOnRide : ""
}

getMetroConfig :: String -> MetroConfig
getMetroConfig city = do
    let config = fetchRemoteConfigString "metro_configs"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultMetroConfig
    getCityBasedConfig value city

defaultMetroConfig :: MetroConfig
defaultMetroConfig = {
  tnc : "",
  logoImage : "",
  mapImage : "",
  bannerImage : "",
  bannerBackgroundColor : "",
  bannerTextColor : "",
  showCancelButton : false
}

defaultBoostSearchConfig :: BoostSearchConfig
defaultBoostSearchConfig = {
    selectedEstimates : [],
    selectedTip : 0
}

defaultVariantBasedBoostSearchConfig :: VariantBasedBoostSearchConfig
defaultVariantBasedBoostSearchConfig = {
  sedan : defaultBoostSearchConfig,
  suv : defaultBoostSearchConfig,
  hatchback : defaultBoostSearchConfig,
  autoRickshaw : defaultBoostSearchConfig,
  taxi : defaultBoostSearchConfig,
  taxiPlus : defaultBoostSearchConfig,
  bike : defaultBoostSearchConfig,
  suvPlus : defaultBoostSearchConfig,
  default : defaultBoostSearchConfig,
  bookAny : defaultBoostSearchConfig
}

getBoostSearchConfig :: String -> String -> BoostSearchConfig
getBoostSearchConfig city variant =
    let config = fetchRemoteConfigString "boost_search_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultVariantBasedBoostSearchConfig
        cityConfig = getCityBasedConfig value city
    in case variant of
        "SEDAN" -> cityConfig.sedan
        "SUV" -> cityConfig.suv
        "SUV_PLUS" -> cityConfig.suvPlus
        "HATCHBACK" -> cityConfig.hatchback
        "AUTO_RICKSHAW" -> cityConfig.autoRickshaw
        "TAXI" -> cityConfig.taxi
        "TAXI_PLUS" -> cityConfig.taxiPlus
        "BOOK_ANY" -> cityConfig.bookAny
        "DELIVERY_BIKE" -> cityConfig.bike
        _ -> cityConfig.default




cancelBookingReasonsConfigData :: String -> Boolean -> Array OptionButtonList
cancelBookingReasonsConfigData  language showAcReason  =
  let
    remoteConfig =  fetchRemoteConfigString ("cancel_booking_reasons_config_" <> language)
    utilsConfig = cancelReasons showAcReason
    (decodedConfg::RemoteCancellationReason) = decodeForeignAny (parseJSON remoteConfig) $ {cancellationReasons : defaultCancelReasons}
    config =utilsConfig <> decodedConfg.cancellationReasons
  in
    config


defaultCancelReasons :: Array OptionButtonList
defaultCancelReasons =
  [  { reasonCode: ""
  , description: ""
  , textBoxRequired: false
  , subtext: Nothing
  }]

cancelReasons :: Boolean -> Array OptionButtonList
cancelReasons showAcReason =
  ([ { reasonCode: "CHANGE_OF_PLANS"
    , description: getString CHANGE_OF_PLANS
    , subtext: Just $ getString NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS
    , textBoxRequired : false
    }
  ]) <>
  (if showAcReason
      then [ { reasonCode: "AC_NOT_TURNED_ON"
              , description: getString AC_IS_NOT_AVAILABLE_ON_THIS_RIDE
              , subtext: Just $ getString AC_NOT_WORKING_DESC
              , textBoxRequired : false
            }]
      else []
  )
eventsConfig :: String -> Types.EventsConfig
eventsConfig key =
    let stringifiedConf = fetchRemoteConfigString key
    in decodeForeignObject (parseJSON stringifiedConf) defEventsConfig

defEventsConfig :: Types.EventsConfig
defEventsConfig = {
  enabled : false,
  pushEventChunkSize : 10,
  loggingIntervalInMs : 10000.0
}

getMapViewLottieConfig :: LazyCheck -> MapLottieConfig
getMapViewLottieConfig _ =
    let config = fetchRemoteConfigString "map_view_lottie"
        value = decodeForeignObject (parseJSON config) defaultMapLottieConfig
    in value

defaultMapLottieConfig :: MapLottieConfig
defaultMapLottieConfig = {
    lottieUrl : "",
    visibility : false
}

defaultCancellationBannerThresholdConfig :: CancellationThreshold
defaultCancellationBannerThresholdConfig = {
    showBanner : false,
    percentage : 100.0
}

getCancellationBannerThresholdConfig :: String -> CancellationThreshold
getCancellationBannerThresholdConfig city =
    let config = fetchRemoteConfigString "customer_cancellation_banner_threshold"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultCancellationBannerThresholdConfig
    in getCityBasedConfig value $ DS.toLower city



defaultGetEnquiryBannerConfig :: EnquiryBannerConfigs
defaultGetEnquiryBannerConfig = {
    question : Nothing,
    firstBtnBanner: Nothing,
    secondBtnBanner: Nothing,
    categoryId: "",
    optionId: Nothing
}


defaultGetVehicleEnquiryBannerConfig :: VehicleEnquiryBannerConfigs
defaultGetVehicleEnquiryBannerConfig = {
  auto : Nothing
, car : Nothing
, ambulance : Nothing
, bike : Nothing
}


data VehicleCategory = AutoCategory | BikeCategory | AmbulanceCategory | CarCategory

getCategoryFromVariant :: String -> VehicleCategory
getCategoryFromVariant variant = case variant of
  _ | DA.elem variant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> AutoCategory
  "BIKE" -> BikeCategory
  _ | variant `DA.elem` ["AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"] -> AmbulanceCategory
  _ -> CarCategory


getEnquiryBannerConfig :: String -> VehicleCategory -> Maybe EnquiryBannerConfigs
getEnquiryBannerConfig city mbVehicleCat =
    let
        cachedValue = runFn3 getFromCache "customer_enquiry_config_v2" Nothing Just
        finalValue = case cachedValue of
            Just value -> value
            Nothing ->
                let
                    config = fetchRemoteConfigString "customer_enquiry_config_v2"
                    value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultGetVehicleEnquiryBannerConfig
                    resp = getCityBasedConfig value $ DS.toLower city
                    _ = runFn2 setInCache "customer_enquiry_config_v2" resp
                in
                    resp
    in
        case mbVehicleCat of
            AutoCategory -> finalValue.auto
            BikeCategory -> finalValue.bike
            AmbulanceCategory -> finalValue.ambulance
            CarCategory -> finalValue.car


defaultReferralPayout :: ReferralPayoutConfig
defaultReferralPayout = {
    enable : Just true,
    youGet : Nothing,
    theyGet : Nothing,
    coverImage : Nothing,
    termsLink : "https://docs.google.com/document/d/1ZkgiluudQnYiFWlMNEZ18owFdkz_LqDr"
}

getReferralPayoutConfig :: String -> ReferralPayoutConfig
getReferralPayoutConfig city = do
    let config = fetchRemoteConfigString "referral_payout_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultReferralPayout
    getCityBasedConfig value $ DS.toLower city

getEnableTips :: String -> Boolean
getEnableTips city = do
    let config = fetchRemoteConfigString "enable_tips_estimates"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig false
    getCityBasedConfig value $ DS.toLower city
