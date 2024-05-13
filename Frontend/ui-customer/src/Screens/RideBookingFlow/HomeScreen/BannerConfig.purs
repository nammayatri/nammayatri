module Screens.RideBookingFlow.HomeScreen.BannerConfig where

import Prelude

import Common.Types.App (LazyCheck(..), ProviderType(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..))
import Components.BannerCarousel as BannerCarousel
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Helpers.Utils (FetchImageFrom(..), fetchImage, getAssetLink, getCommonAssetLink, getMetroConfigFromCity, CityMetroConfig(..), getCityConfig)
import Language.Types (STR(..))
import Screens.Types (City, HomeScreenState)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Locale.Utils (getLanguageLocale, languageKey)
import Data.String (null, take, toLower)
import Prelude (not, show, ($), (&&), (<>), (==))
import Locale.Utils (getLanguageLocale)
import SessionCache (getValueFromWindow)
import RemoteConfig as RC
import Debug
import MerchantConfig.Types (CityConfig)
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)


getBannerConfigs :: forall action. HomeScreenState -> (BannerCarousel.Action -> action) -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
getBannerConfigs state action =
  (if state.props.city == ST.Chennai || state.props.city == ST.Kochi
  then [metroBannerConfig state action]
  else [])
  <> (if state.data.config.banners.homeScreenCabLaunch && state.props.city == ST.Bangalore then [cabLaunchBannerConfig state action] else [])
  <> (if state.data.config.feature.enableAdditionalServices || (cityConfig state).enableRentals
  then [checkoutRentalBannerConfig state action] 
  else [])
  <> 
  (if (getValueToLocalStore DISABILITY_UPDATED == "false" && state.data.config.showDisabilityBanner) 
    then [disabilityBannerConfig state action] 
    else [])
  <> (if  state.data.config.banners.homeScreenSafety && state.props.sosBannerType == Just ST.SETUP_BANNER && state.data.config.feature.enableSafetyFlow then [sosSetupBannerConfig state action] else [])
  <> (getRemoteBannerConfigs state.props.city)
  where
    getRemoteBannerConfigs :: City -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
    getRemoteBannerConfigs city = do
      let location = toLower $ show city
          language = getLanguage $ getLanguageLocale languageKey
          configName = "customer_carousel_banner" <> language
          datas = RC.carouselConfigData location configName "customer_carousel_banner_en" (getValueFromWindow "CUSTOMER_ID") ""
      BannerCarousel.remoteConfigTransformer datas action
    getLanguage :: String -> String
    getLanguage lang = 
      let language = toLower $ take 2 lang
      in if not (null language) then "_" <> language else "_en"

    cityConfig :: HomeScreenState -> CityConfig
    cityConfig state = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)


getDriverInfoCardBanners :: forall action. HomeScreenState -> (BannerCarousel.Action -> action) -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
getDriverInfoCardBanners state action = 
  if isJust state.props.sosBannerType && state.data.config.feature.enableSafetyFlow && state.data.driverInfoCardState.providerType == ONUS
    then [sosSetupBannerConfig state action] 
    else []


disabilityBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
disabilityBannerConfig state action =
  let
    config = BannerCarousel.config action
    config' = config
      {
        backgroundColor = Color.paleLavender
      , title = (getString NOW_GET_ASSISTED_RIDES)
      , titleColor = Color.purple
      , actionText = (getString UPDATE_PROFILE)
      , actionTextColor = Color.purple
      , imageUrl = "ny_ic_accessibility_banner_img"
      , type = BannerCarousel.Disability
      }
  in config'
  
sosSetupBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
sosSetupBannerConfig state action =
  let
    config = BannerCarousel.config action

    bannerConfig =
      case state.props.sosBannerType of
        Just ST.SETUP_BANNER -> {title: getString COMPLETE_YOUR_NAMMA_SAFETY_SETUP_FOR_SAFE_RIDE_EXPERIENCE, actionText: getString SETUP_NOW, image : "ny_ic_banner_sos_red", backgroundColor: Color.red200, color : Color.darkRed, shieldImage: "ny_ic_shield_red"}
        Just ST.MOCK_DRILL_BANNER -> {title: getString COMPLETE_YOUR_TEST_DRILL, actionText: getString TEST_DRILL, image : "ny_ic_mock_drill_banner_blue", backgroundColor: Color.azureWhite, color : Color.policeBlue, shieldImage: "ny_ic_shield_blue"}
        Nothing -> {title: "", actionText: "", image : "", backgroundColor: "", color : "", shieldImage: ""}
    config' =
      config
        { backgroundColor = bannerConfig.backgroundColor
        , title = bannerConfig.title
        , titleColor = bannerConfig.color
        , actionText = bannerConfig.actionText
        , actionTextColor = Color.white900
        , actionTextBackgroundColour = bannerConfig.color
        , imageUrl = (getAssetLink FunctionCall) <> bannerConfig.image <> ".png"
        , actionIconUrl = (getAssetLink FunctionCall) <> bannerConfig.shieldImage <> ".png"
        , actionIconVisibility = true
        , actionArrowIconVisibility = false
        , type = BannerCarousel.Safety
        }
  in
    config'

cabLaunchBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
cabLaunchBannerConfig state action =
  let
    config = BannerCarousel.config action
    config' =
      config
        { backgroundColor = Color.lightCyan
        , title = "We have a cab-ulous surprise for you! 🚖"
        , titleColor = Color.azureine
        , actionText = "Book a cab"
        , actionTextBackgroundColour = Color.azureine
        , actionTextColor = Color.white900
        , imageUrl = (getAssetLink FunctionCall) <> "ny_ic_cab_launch.png"
        , margin = MarginTop 0
        , imageHeight = V 100
        , imageWidth = V 120
        , padding = Padding 0 2 5 5
        , imagePadding = PaddingLeft 24
        , type = BannerCarousel.CabLaunch
        , actionArrowIconVisibility = false
        }
  in
    config'

metroBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
metroBannerConfig state action =
  let
    config = BannerCarousel.config action
    (CityMetroConfig cityConfig) = getMetroConfigFromCity state.props.city 
    config' = config
      {
        backgroundColor = Color.blue600'
      , title = getString BOOK_METRO_WITH_NY_NOW
      , titleColor = Color.blue800
      , actionText = getString BOOK_NOW
      , actionTextBackgroundColour = Color.blue800
      , actionTextColor = Color.white900
      , imageUrl = (getCommonAssetLink FunctionCall) <> (cityConfig.bannerImage <> ".png")
      , margin = MarginTop 0
      , imageHeight = V 100
      , imageWidth = V 120
      , padding = Padding 0 2 5 5
      , imagePadding = PaddingLeft 24
      , type = BannerCarousel.MetroTicket
      }
  in config'

ticketBannerConfig :: forall action. ST.HomeScreenState -> action -> BannerCarousel.Config action
ticketBannerConfig state action =
  let
    config = BannerCarousel.config action
    config' = config
      {
        backgroundColor = "#FFF6DE"
      , title = "Book Millennium Jetty, Heritage cruise and Alipore  zoo tickets "
      , titleColor = Color.black800
      , actionText = "Book Now"
      , actionTextColor = Color.black900
      , imageUrl = fetchImage FF_ASSET "ny_ic_zoo_banner"
      , margin = MarginTop 0
      , imageHeight = V 75
      , imageWidth = V 60
      , padding = Padding 0 5 5 5
      , type = BannerCarousel.ZooTicket
      }
  in config'

checkoutRentalBannerConfig :: forall action. ST.HomeScreenState -> action -> BannerCarousel.Config action
checkoutRentalBannerConfig state action =
  let
    config = BannerCarousel.config action
    appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
    config' = config
      {
        backgroundColor = Color.blue600
      , title = getString $ RENTALS_INTERCITY_AVAILABLE appName
      , titleColor = Color.blue800
      , actionText = getString CHECK_IT_OUT
      , actionTextBackgroundColour = Color.blue800
      , actionTextColor = Color.white900
      , imageUrl = fetchImage FF_ASSET "ny_ic_rental_banner"
      , actionArrowIconVisibility = false
      , actionBottomArrowIconVisibility = true
      , margin = MarginTop 0
      , imageHeight = V 96
      , imageWidth = V 80
      , actionIconUrl = (getAssetLink FunctionCall) <> "ny_ic_shield_red.png"
      , padding = Padding 0 2 5 5
      , imagePadding = PaddingVertical 0 0
      , type = BannerCarousel.RentalsAndIntercity
      }
  in config'