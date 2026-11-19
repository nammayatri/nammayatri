{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.Controller where

import Prelude (class Show, pure, ($), show, not, map, (<>), (==), (+), (>=), (&&), (||), (/=), (>), discard, void, unit)
import PrestoDOM (Eval, defaultPerformLog, exit, continue,continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM.Core (processEvent)
import Screens.Types (NammaSafetyScreenState, SafetyStepsConfig(..), NewContacts(..), NammaSafetyStage(..))
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.LargeBannerCarousel as LargeBannerCarousel
import RemoteConfig as RC
import Data.String as DS
import Effect.Unsafe (unsafePerformEffect)
import Locale.Utils (getLanguageLocale)
import Constants (languageKey, globalPayload)
import Data.Array as Array
import Data.Int as DI
import Data.Maybe (Maybe(..), fromMaybe)
import PrestoDOM.List (ListItem)
import Services.API as API
import Helpers.Utils (emitTerminateApp)
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Debug
import Screens.DataExplainWithFetch.ComponentConfig (getBooleanFromOptions)
import Engineering.Helpers.Commons (getGlobalPayload)
import Data.Lens ((^.))
import Mobility.Prelude as MP
import Engineering.Helpers.Accessor


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = Exit
  | Navigate NammaSafetyScreenState SafetyStepsConfig

data Action
  = AfterRender
  | GenericHeaderActionController GenericHeaderController.Action
  | BackPressed
  | UpdateBanner
  | BannerChanged String
  | BannerStateChanged String
  | BannerCarousel LargeBannerCarousel.Action
  | SetBannerItem ListItem
  | SafetyNavigation SafetyStepsConfig
  | UpdateSafetySettings API.GetEmergencySettingsRes

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval AfterRender state = continue state

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval BackPressed state = do
  let mBPayload = getGlobalPayload globalPayload
  case mBPayload of
    Just globalPayload ->
      case globalPayload ^. _payload ^. _view_param of
        Just screen ->
          if MP.startsWith "rideConfirmed" screen
            then exit Exit
            else do
              void $ pure $ emitTerminateApp Nothing true
              continue state
        Nothing -> do
          void $ pure $ emitTerminateApp Nothing true
          continue state
    Nothing -> do
      void $ pure $ emitTerminateApp Nothing true
      continue state


eval (SetBannerItem bannerItem) state = continue state { data { bannerData { bannerItem = Just bannerItem } } }

eval (SafetyNavigation navigationConfig) state = exit $ Navigate state navigationConfig

eval (UpdateSafetySettings settingsResp) state =
  let
    safetyContacts = transformContactDetailsToNewContacts settingsResp
  in
    continue state { data { settingsAPIResponse = settingsResp, emergencyContactsList = safetyContacts, safetySetupSteps = updateSafetySetupSteps state settingsResp (Array.length safetyContacts) } }

eval UpdateBanner state = do
  if state.data.bannerData.bannerScrollState == "1" then
    continue state
  else do
    let
      nextBanner = state.data.bannerData.currentBanner + 1

      bannerArray = getBannerConfigs state BannerCarousel

      updatedIdx = if nextBanner >= (Array.length bannerArray) then 0 else nextBanner

      newState = state { data { bannerData { currentBanner = updatedIdx, currentPage = updatedIdx } } }
    continue newState

eval (BannerChanged item) state = do
  let
    currentBanner = DI.fromString item
  case currentBanner of
    Just idx -> do
      let
        newState = state { data { bannerData { currentBanner = idx } } }
      if state.data.bannerData.currentPage /= idx then
        void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit -- To stop and stop the new autosroll
      else
        pure unit
      continue newState
    Nothing -> continue state

eval (BannerStateChanged item) state = do
  let
    newState = state { data { bannerData { bannerScrollState = item } } }
  continue newState

eval _ state = continue state

updateSafetySetupSteps :: NammaSafetyScreenState -> API.GetEmergencySettingsRes -> Int -> Array SafetyStepsConfig
updateSafetySetupSteps state (API.GetEmergencySettingsRes settingsResp) contactsLength =
  map
    ( \stepsConfig -> case stepsConfig.navigation of
        TrustedContacts _ -> stepsConfig { isCompleted = contactsLength > 0 }
        SafetyCheckIn _ -> stepsConfig { isCompleted = getBooleanFromOptions settingsResp.enableUnexpectedEventsCheck || getBooleanFromOptions settingsResp.enablePostRideSafetyCheck || settingsResp.informPoliceSos || settingsResp.notifySafetyTeamForSafetyCheckFailure }
        EmergencyActions _ -> stepsConfig { isCompleted = settingsResp.shakeToActivate || settingsResp.autoCallDefaultContact }
        SafetyDrill _ -> stepsConfig { isCompleted = settingsResp.hasCompletedMockSafetyDrill}
        TrustedContactsActions _ -> stepsConfig
        DriverSafetyStandards _ -> stepsConfig
    )
    state.data.safetySetupSteps

getBannerConfigs :: forall action. NammaSafetyScreenState -> (LargeBannerCarousel.Action -> action) -> Array (LargeBannerCarousel.Config (LargeBannerCarousel.Action -> action))
getBannerConfigs state action = getRemoteBannerConfigs "Bangalore"
  where
  getRemoteBannerConfigs :: String -> Array (LargeBannerCarousel.Config (LargeBannerCarousel.Action -> action))
  getRemoteBannerConfigs city = do
    let
      location = DS.toLower $ show city

      language = getLanguage $ getLanguageLocale languageKey

      configName = "customer_carousel_banner" <> language
    LargeBannerCarousel.remoteConfigTransformer getSafetyCarouselData action $ Just "large"

  getLanguage :: String -> String
  getLanguage lang =
    let
      language = DS.toLower $ DS.take 2 lang
    in
      if not (DS.null language) then "_" <> language else "_en"

  getSafetyCarouselData :: Array RC.RCCarousel
  getSafetyCarouselData =
    [ RC.RCCarousel
        { text_color: ""
        , text: ""
        , cta_text: ""
        , cta_action: Nothing
        , cta_link: ""
        , cta_icon: ""
        , accessibilityHint: Nothing
        , banner_color: ""
        , banner_image: ""
        , cta_background_color: ""
        , cta_text_color: ""
        , cta_corner_radius: ""
        , cta_image_url: ""
        , whitelist: Nothing
        , categoryFilter: Nothing
        , image_banner: Just "https://assets.moving.tech/beckn/common/common/images/ny_ic_trusted_contacts_carousel.png"
        , dynamic_action: Nothing
        , showDuringRide : Nothing
        }
    , RC.RCCarousel
        { text_color: ""
        , text: ""
        , cta_text: ""
        , cta_action: Nothing
        , accessibilityHint: Nothing
        , cta_link: ""
        , cta_icon: ""
        , banner_color: ""
        , banner_image: ""
        , cta_background_color: ""
        , cta_text_color: ""
        , cta_corner_radius: ""
        , cta_image_url: ""
        , whitelist: Nothing
        , categoryFilter: Nothing
        , image_banner: Just "https://assets.moving.tech/beckn/common/common/images/ny_ic_safety_checks_carousel.png"
        , dynamic_action: Nothing
        , showDuringRide : Nothing
        }
    , RC.RCCarousel
        { text_color: ""
        , text: ""
        , cta_text: ""
        , cta_action: Nothing
        , cta_link: ""
        , cta_icon: ""
        , accessibilityHint: Nothing
        , banner_color: ""
        , banner_image: ""
        , cta_background_color: ""
        , cta_text_color: ""
        , cta_corner_radius: ""
        , cta_image_url: ""
        , whitelist: Nothing
        , categoryFilter: Nothing
        , image_banner: Just "https://assets.moving.tech/beckn/common/common/images/ny_ic_emergency_actions_carousel.png"
        , dynamic_action: Nothing
        , showDuringRide : Nothing
        }
    , RC.RCCarousel
        { text_color: ""
        , text: ""
        , cta_text: ""
        , cta_action: Nothing
        , cta_link: ""
        , accessibilityHint: Nothing
        , cta_icon: ""
        , banner_color: ""
        , banner_image: ""
        , cta_background_color: ""
        , cta_text_color: ""
        , cta_corner_radius: ""
        , cta_image_url: ""
        , whitelist: Nothing
        , categoryFilter: Nothing
        , image_banner: Just "https://assets.moving.tech/beckn/common/common/images/ny_ic_test_drill_carousel.png"
        , dynamic_action: Nothing
        , showDuringRide : Nothing
        }
    ]

transformContactDetailsToNewContacts :: API.GetEmergencySettingsRes -> Array NewContacts
transformContactDetailsToNewContacts (API.GetEmergencySettingsRes settingsResp) =
  map
    ( \(API.ContactDetails contact) ->
        let
          mbPriority = fromMaybe 1 contact.priority
        in
          { name: contact.name
          , number: contact.mobileNumber
          , isSelected: mbPriority == 0
          , enableForFollowing: fromMaybe false contact.enableForFollowing
          , enableForShareRide: fromMaybe false contact.enableForShareRide
          , onRide: fromMaybe false contact.onRide
          , priority: mbPriority
          , shareTripWithEmergencyContactOption: EmergencyContactsScreenData.getRideOptionFromKeyEM $ fromMaybe API.NEVER_SHARE contact.shareTripWithEmergencyContactOption
          , contactPersonId: contact.contactPersonId
          , isFollowing: Nothing
          , notifiedViaFCM: Nothing
          }
    )
    settingsResp.defaultEmergencyNumbers
