module Screens.DriverClaimRewardScreen.Controller
  ( Action(..)
  , ScreenOutput(..)
  , eval
  , getVideoBannerConfigs
  )
  where

import Prelude (class Show, pure, unit, ($), not, void,discard ,(<>) ,show , (==), (+),(>=),(/=),map)
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, toPropValue)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (DriverClaimRewardScreenState)
import JBridge (openUrlInApp)
import Storage (getValueToLocalStore, KeyStore(..))
import Helpers.Utils (contactSupportNumber)
import Data.Maybe
import Effect.Unsafe (unsafePerformEffect)
import PrestoDOM.Core (processEvent)
import Data.Int (fromString)
import Data.Array (length)
import Data.Array as Array
import Data.String
import Components.BannerCarousel as BannerCarousel
import Locale.Utils(getLanguageLocale)
import Constants (languageKey)
import RemoteConfig as RC
import SessionCache (getValueFromWindow)
import Data.Function.Uncurried (runFn2)
import Engineering.Helpers.Commons as EHC
import PrestoDOM.List (ListItem)

instance showAction :: Show Action where
  show BackPressed = "BackPressed"
  show ViewMoreBenefits = "ViewMoreBenefits"
  show ViewMoreEligibility = "ViewMoreEligibility"
  show (YoutubeVideoStatus _) = "YoutubeVideoStatus"
  show OpenWhatsAppSupport = "OpenWhatsAppSupport"
  show NoAction = "NoAction"
  show FaqClicked = "FaqClicked"
  show CallSupport = "CallSupport"
  show (FaqToggle i) = "FaqToggle " <> show i
  show (BenefitToggle i) = "BenefitToggle" <> show i
  show UpdateBanner = "UpdateBanner"
  show (BannerChanged item) = "BannerChanged"
  show (BannerStateChanged _) = "BannerStateChanged"
  show (BannerCarousal item) = "BannerCarousal" <> show item
  show (SetBannerItem _) = "SetBannerItem"

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> pure unit
    ViewMoreBenefits -> pure unit
    ViewMoreEligibility -> pure unit
    YoutubeVideoStatus _ -> pure unit
    OpenWhatsAppSupport -> pure unit
    NoAction -> pure unit
    FaqClicked -> pure unit
    FaqToggle _ -> pure unit
    CallSupport -> pure unit
    BenefitToggle _ -> pure unit
    UpdateBanner -> pure unit
    BannerChanged _ -> pure unit
    BannerStateChanged _ -> pure unit
    BannerCarousal _ -> pure unit
    SetBannerItem _ -> pure unit
    _ -> pure unit

data Action = BackPressed | ViewMoreBenefits | ViewMoreEligibility | YoutubeVideoStatus String | NoAction | OpenWhatsAppSupport | FaqClicked | FaqToggle Int | CallSupport | BenefitToggle Int | UpdateBanner | BannerChanged String | BannerStateChanged String | BannerCarousal BannerCarousel.Action | SetBannerItem ListItem

data ScreenOutput = GoBack

eval :: Action -> DriverClaimRewardScreenState -> Eval Action ScreenOutput DriverClaimRewardScreenState

eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}}

eval BackPressed state = if state.props.showFaq then continue state { props { showFaq = false }} else exit GoBack

eval ViewMoreBenefits state = continue state { props { showAllBenefits = not state.props.showAllBenefits } }

eval ViewMoreEligibility state = continue state { props { showAllEligibility = not state.props.showAllEligibility } }

eval OpenWhatsAppSupport state = continueWithCmd state [do
  let supportPhone = state.data.driverRewardConfig.whatsappSupportNumber
  let name = "%0AName%3A%20" <> getValueToLocalStore USER_NAME
      phone = "%0APhone%20Number%3A%20" <> getValueToLocalStore MOBILE_NUMBER_KEY
      message = "Hi%2C%20I%27d%20like%20to%20file%20a%20claim%20under%20the%20Namma%20Kutumba%20program.%0AMy%20details%20are%20as%20follows%3A" <> name <> phone
      url = "https://wa.me/" <> supportPhone <> "?text=" <> message
  void $ openUrlInApp url
  pure NoAction
]

eval FaqClicked state = continue state {props {showFaq = true}}

eval CallSupport state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state

eval (FaqToggle i) state =
  let current = state.props.openFaqIndex
      newIndex = if current == Just i then Nothing else Just i
  in continue state { props { openFaqIndex = newIndex } }

eval (BenefitToggle i) state =
  let current = state.props.openBenefitIndex
      newIndex = if current == Just i then Nothing else Just i
  in continue state { props { openBenefitIndex = newIndex } }

eval UpdateBanner state = do
  if state.data.bannerData.bannerScrollState == "1" then continue state
  else do
    let nextBanner = state.data.bannerData.currentBanner + 1
        updatedIdx = if nextBanner >= Array.length (getVideoBannerConfigs state) then 0 else nextBanner
        newState = state{data {bannerData{currentBanner = updatedIdx, currentPage = updatedIdx}}}
    continue newState

eval (BannerChanged item) state = do
  let currentBanner = fromString item
  case currentBanner of
    Just idx -> do
        let newState = state{data {bannerData{currentBanner = idx}}}
        if state.data.bannerData.currentPage /= idx then void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit -- To stop and stop the new autosroll
          else pure unit
        continue newState
    Nothing  -> continue state

eval (BannerStateChanged item) state = do
  let newState = state{data {bannerData{bannerScrollState = item}}}
  update newState

eval (BannerCarousal (BannerCarousel.OnClick index)) state =
  continueWithCmd state [do
    let banners = getVideoBannerConfigs state
    case Array.index banners index of
      Just config -> do
        let _ = runFn2 EHC.updatePushInIdMap "bannerCarousel" false
        case config.type of
          BannerCarousel.Remote link -> do
            void $ openUrlInApp link
            pure NoAction
          _ -> pure NoAction
      Nothing -> pure NoAction
  ]
eval _ state = update state 

getVideoBannerConfigs :: DriverClaimRewardScreenState -> Array (BannerCarousel.Config (BannerCarousel.Action -> Action))
getVideoBannerConfigs state =
  let
    defaultConfig = BannerCarousel.config BannerCarousal
    transform item =
      defaultConfig
        { type = BannerCarousel.Remote item.videoUrl
        , imageBannerUrl = item.imageUrl
        , bannerSize = Just "large"
        }
  in
    map transform state.data.driverRewardConfig.carousel