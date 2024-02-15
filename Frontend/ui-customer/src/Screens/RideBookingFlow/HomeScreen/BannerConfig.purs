module Screens.RideBookingFlow.HomeScreen.BannerConfig where

import Prelude

import Common.Types.App (LazyCheck(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..))
import Components.BannerCarousel as BannerCarousel
import Data.Maybe (Maybe(..), isJust)
import Helpers.Utils (FetchImageFrom(..), fetchImage, getAssetLink)
import Language.Types (STR(..))
import Screens.Types (City, HomeScreenState)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Locale.Utils (getLanguageLocale, languageKey)
import Data.String (null, take, toLower)
import Prelude (not, show, ($), (&&), (<>), (==))
import Locale.Utils (getLanguageLocale)
import RemoteConfig as RC

getBannerConfigs :: forall action. HomeScreenState -> (BannerCarousel.Action -> action) -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
getBannerConfigs state action =
  (if state.props.city == ST.Chennai
  then [metroBannerConfig state action]
  else [])
  <>
  (if isJust state.props.sosBannerType && state.data.config.feature.enableSafetyFlow
    then [sosSetupBannerConfig state action] 
    else [])
  <>
  (if (getValueToLocalStore DISABILITY_UPDATED == "false" && state.data.config.showDisabilityBanner) 
    then [disabilityBannerConfig state action] 
    else [])
  <> (if (state.data.config.feature.enableZooTicketBookingFlow)
    then [ticketBannerConfig state action] else [])
  <> (getRemoteBannerConfigs state.props.city)
  where
    getRemoteBannerConfigs :: City -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
    getRemoteBannerConfigs city = do
      let location = toLower $ show city
          language = getLanguage $ getLanguageLocale languageKey
          configName = "customer_carousel_banner" <> language
          datas = RC.carouselConfigData location configName "customer_carousel_banner_en"
      BannerCarousel.remoteConfigTransformer datas action
    getLanguage :: String -> String
    getLanguage lang = 
      let language = toLower $ take 2 lang
      in if not (null language) then "_" <> language else "_en"


getDriverInfoCardBanners :: forall action. HomeScreenState -> (BannerCarousel.Action -> action) -> Array (BannerCarousel.Config (BannerCarousel.Action -> action))
getDriverInfoCardBanners state action = 
  if isJust state.props.sosBannerType && state.data.config.feature.enableSafetyFlow
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
        Just ST.SETUP_BANNER -> {title: getString COMPLETE_YOUR_NAMMA_SAFETY_SETUP_FOR_SAFE_RIDE_EXPERIENCE, actionText: getString SETUP_NOW, image : "ny_ic_banner_sos_red"}
        Just ST.MOCK_DRILL_BANNER -> {title: getString COMPLETE_YOUR_TEST_DRILL, actionText: getString TEST_DRILL, image : "ny_ic_mock_drill_banner_red"}
        Nothing -> {title: "", actionText: "", image : ""}
    config' =
      config
        { backgroundColor = Color.red200
        , title = bannerConfig.title
        , titleColor = Color.darkRed
        , actionText = bannerConfig.actionText
        , actionTextColor = Color.white900
        , actionTextBackgroundColour = Color.darkRed
        , imageUrl = (getAssetLink FunctionCall) <> bannerConfig.image <> ".png"
        , actionIconUrl = (getAssetLink FunctionCall) <> "ny_ic_shield_red.png" 
        , actionIconVisibility = true
        , actionArrowIconVisibility = false
        , type = BannerCarousel.Safety
        }
  in
    config'



metroBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
metroBannerConfig state action =
  let
    config = BannerCarousel.config action
    config' = config
      {
        backgroundColor = Color.blue600'
      , title = getString BOOK_METRO_WITH_NY_NOW
      , titleColor = Color.blue800
      , actionText = getString BOOK_NOW
      , actionTextBackgroundColour = Color.blue800
      , actionTextColor = Color.white900
      , imageUrl = (getAssetLink FunctionCall) <> "ny_ic_metro_banner.png"
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