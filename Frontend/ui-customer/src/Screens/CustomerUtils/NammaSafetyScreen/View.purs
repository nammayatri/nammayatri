{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyScreen.View where

import PrestoDOM
import Screens.NammaSafetyScreen.ComponentConfig

import Animation (fadeIn)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, null, (!!), (..))
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, requestCameraAndMicrophonePermissions)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Mobility.Prelude (boolToVisibility)
import Prelude
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import Screens.EmergencyContactsScreen.Controller (contactColorsList)
import Screens.EmergencyContactsScreen.View (getFirstChar, getLastChar)
import Screens.NammaSafetyScreen.Controller (Action(..), ScreenOutput, eval, checkForContactsAndSupportDisabled)
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..), NewContacts, RecordingState(..), StepsHeaderModelState)
import Screens.Types as ST
import Services.API (GetSosDetailsRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "NammaSafetyScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  response <- Remote.getEmergencySettingsBT ""
                  lift $ lift $ doAff do liftEffect $ push $ UpdateEmergencySettings response
                  if initialState.data.sosId == "" && initialState.props.onRide then do
                    (GetSosDetailsRes sosDetails) <- Remote.getSosDetails initialState.data.rideId
                    case sosDetails.sosId of
                      Just id -> do
                        lift $ lift $ doAff do liftEffect $ push $ UpdateSosId id
                        pure unit
                      Nothing -> pure unit
                  else if not checkForContactsAndSupportDisabled initialState
                    && initialState.props.currentStage == ActivateNammaSafety
                    && initialState.props.onRide then do
                    lift $ lift $ doAff do liftEffect $ push $ SwitchToStage TriggeredNammaSafety
                  else
                    pure unit
                  lift $ lift $ doAff do liftEffect $ push $ DisableShimmer
                  lift $ lift $ EHU.toggleLoader false
                  pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "NammaSafety action " action
        let
          _ = spy "NammaSafety state " state
        eval action state
  }

view ::
  forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background background'
    , padding padding'
    ]
    [ headerView state push
    , frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , visibility $ boolToVisibility $ not state.props.showShimmer
        ]
        [ dashboardView state push (state.props.currentStage == NammaSafetyDashboard)
        , aboutNammaSafetyView state push (state.props.currentStage == AboutNammaSafety)
        , settingUpView state push (state.props.currentStage == SetTriggerCustomerSupport || state.props.currentStage == SetNightTimeSafetyAlert || state.props.currentStage == SetDefaultEmergencyContacts || state.props.currentStage == SetPersonalSafetySettings)
        , educationView state (state.props.currentStage == EduNammaSafetyMeasures || state.props.currentStage == EduNammaSafetyGuidelines || state.props.currentStage == EduNammaSafetyAboutSOS)
        , activateNammaSafetyView state push (state.props.currentStage == ActivateNammaSafety)
        , sosActiveView state push (state.props.currentStage == TriggeredNammaSafety)
        , if state.props.showInfoPopUp then removeContactPopUpView push state else emptyTextView
        , if state.props.confirmPopup then PopUpModal.view (push <<< ConfirmSOSActivate) (confirmPopUpModelConfig state) else emptyTextView
        ]
    , shimmerView state
    ]
  where
  background' =
    if any (_ == state.props.currentStage) [ ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord ] then
      Color.black900
    else
      Color.white900

  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

-- ---------------------------------- dashboardView -----------------------------------
dashboardView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
dashboardView state push visibility' =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ]
            []
        , nammaSafetyFeaturesView state push showFeatures
        , userSettingsView state push $ not showFeatures
        ]
    ]
  where
  showFeatures = state.data.hasCompletedSafetySetup == false && state.props.onRide == false

-- ---------------------------------- headerView -----------------------------------
headerView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility visibility'
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , weight 1.0
            ]
            [ GenericHeader.view (push <<< GenericHeaderAC)
                ( genericHeaderConfig
                    ( getHeaderTitle state.props.currentStage $ isSafetyPlus state
                    )
                    state
                )
            ]
        , textView
            [ text $ getString LEARN_MORE
            , visibility showLearnMore
            , color Color.blue900
            , gravity RIGHT
            , margin $ MarginRight 16
            , onClick push $ const $ SwitchToStage AboutNammaSafety
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ]
        []
    ]
  where
  visibility' =
    boolToVisibility $ not
      $ any (_ == state.props.currentStage)
          [ SetTriggerCustomerSupport, SetNightTimeSafetyAlert, SetDefaultEmergencyContacts, SetPersonalSafetySettings, NammaSafetyVideoRecord ]
      || state.props.recordingState == SHARED

  showLearnMore = boolToVisibility (state.props.currentStage == NammaSafetyDashboard && state.data.hasCompletedSafetySetup || state.props.currentStage == ActivateNammaSafety)

-- ---------------------------------- aboutNammaSafetyView -----------------------------------
aboutNammaSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
aboutNammaSafetyView state push visibility' =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , visibility $ boolToVisibility visibility'
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView
            [ text $ getStringBasedOnMode LEARN_ABOUT_NAMMA_SAFETY $ isSafetyPlus state
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black700
            , background Color.blue600
            , gravity LEFT
            , padding $ Padding 12 16 12 16
            ]
        , cardView state (getCardViewData 0 $ isSafetyPlus state) push
        , cardView state (getCardViewData 1 $ isSafetyPlus state) push
        , cardView state (getCardViewData 2 $ isSafetyPlus state) push
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        ]
        [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
    ]

-- ---------------------------------- nammaSafetyFeaturesView -----------------------------------
nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push visibility' =
  PrestoAnim.animationSet
    [ Anim.fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility visibility'
        ]
        [ featuresView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.white900
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
        ]

featuresView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
featuresView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background viewConfig.backgroundColor
        , gravity CENTER
        , orientation VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 20 16 6
        , stroke $ "1," <> viewConfig.strokeColor
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_shield"
            , width $ V 220
            , height $ V 114
            ]
        , textView
            $ [ text $ getStringBasedOnMode NAMMA_SAFETY_WILL_ENABLE_ACCESS isSafetyPlus'
              , margin $ Margin 16 20 16 4
              , color viewConfig.textColor
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ imageWithTextView (getString $ SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM "SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM") true state.props.currentStage state.data.safetyConfig.enableSupport
            , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_SUPPORT) true state.props.currentStage state.data.safetyConfig.enableSupport
            , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_POLICE) true state.props.currentStage (not state.data.safetyConfig.enableSupport)
            , imageWithTextView (getString SHARE_SOS_SILENTLY_WITH_POLICE) true state.props.currentStage state.props.enableLocalPoliceSupport
            , imageWithTextView (getString ACTIVATE_LIVE_VIDEO_RECORDING_FEATURES) true state.props.currentStage isSafetyPlus'
            , imageWithTextView (getString SHARE_LOCATION_AND_RIDE_DETAILS_EMERGENCY_CONTACT) (state.data.shareToEmergencyContacts || state.props.currentStage /= ActivateNammaSafety) state.props.currentStage true
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility $ boolToVisibility $ state.props.currentStage == ActivateNammaSafety && state.data.shareToEmergencyContacts
            , gravity LEFT
            , margin $ MarginVertical 5 0
            , padding $ PaddingLeft 44
            ]
            (mapWithIndex (\index item -> contactCircleView item index) state.data.contactsList)
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background if state.props.onRide then Color.black700 else Color.white900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ SwitchToStage $ viewConfig.stageToSwitch
            ]
            [ textView
                $ [ textFromHtml viewConfig.actionsText
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background viewConfig.backgroundColor
        , gravity CENTER_VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 12 16 0
        , padding $ Padding 16 16 16 16
        , visibility $ boolToVisibility state.props.enableLocalPoliceSupport
        , stroke $ "1," <> viewConfig.strokeColor
        ]
        [ textView
            $ [ text $ getString OUR_SAFETY_PARTNER
              , color viewConfig.textColor
              , weight 1.0
              ]
            <> FontStyle.tags TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_bangalore_police"
                , width $ V 36
                , height $ V 36
                , margin $ MarginRight 8
                ]
            , textView
                $ [ text $ getString BANGALURU_CITY_POLICE
                  , color viewConfig.textColor
                  ]
                <> FontStyle.tags TypoGraphy
            ]
        ]
    ]
  where
  isSafetyPlus' = isSafetyPlus state

  viewConfig =
    if state.props.currentStage == ActivateNammaSafety then
      { strokeColor: Color.black700
      , backgroundColor: Color.blackOpacity12
      , textColor: Color.white900
      , stageToSwitch: NammaSafetyDashboard
      , actionsText: "<u>" <> getStringBasedOnMode EDIT_ACTIONS isSafetyPlus' <> "</u>"
      }
    else
      { strokeColor: Color.blue600
      , backgroundColor: Color.blue600
      , textColor: Color.black800
      , stageToSwitch: AboutNammaSafety
      , actionsText: "<u>" <> getString LEARN_MORE <> "</u>"
      }

imageWithTextView :: String -> Boolean -> NammaSafetyStage -> Boolean -> forall w. PrestoDOM (Effect Unit) w
imageWithTextView text' isActive stage visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ PaddingHorizontal 16 16
    , margin $ MarginTop 12
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET if isActive then "ny_ic_check" else "ny_ic_ellipse_outline_grey"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        ]
    , textView
        $ [ text text'
          , color color'
          , width $ WRAP_CONTENT
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> FontStyle.tags TypoGraphy
    ]
  where
  color' =
    if stage == ActivateNammaSafety && isActive then
      Color.white900
    else if stage == ActivateNammaSafety then
      Color.black700
    else
      Color.black800

activateNammaSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
activateNammaSafetyView state push visibility' =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ featuresView state push
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        , padding $ PaddingBottom 16
        ]
        [ if checkForContactsAndSupportDisabled state then
            linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , gravity CENTER
              , padding $ PaddingVertical 9 9
              , margin $ Margin 16 0 16 6
              , cornerRadius 8.0
              , background Color.redOpacity20
              , onClick push $ const $ ActivateSoSAndCallPolice
              ]
              [ imageView
                  [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
                  , height $ V 26
                  , width $ V 26
                  , margin $ MarginBottom 8
                  ]
              , textView
                  $ [ text $ getString CALL_POLICE
                    , gravity CENTER
                    , color Color.white900
                    , fontStyle $ FontStyle.semiBold LanguageStyle
                    ]
                  <> FontStyle.paragraphText TypoGraphy
              ]
          else
            PrimaryButton.view (push <<< ActivateSOS) (activateSoSButtonConfig state)
        , PrimaryButton.view (push <<< DismissSOS) (dismissSoSButtonConfig state)
        ]
    ]

cardView :: NammaSafetyScreenState -> CardViewDataType -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
cardView state cardData push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 0
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , onClick push $ const $ SwitchToStage cardData.stage
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET cardData.image
        , height $ V 60
        , margin $ MarginRight 14
        , width $ V 60
        ]
    , textView
        $ [ text cardData.text
          , gravity CENTER_VERTICAL
          , color Color.black800
          ]
        <> FontStyle.body6 TypoGraphy
    ]

type CardViewDataType
  = { stage :: NammaSafetyStage
    , image :: String
    , text :: String
    }

getCardViewData :: Int -> Boolean -> CardViewDataType
getCardViewData index isSafetyPlus = case index of
  0 -> { stage: EduNammaSafetyMeasures, image: "ny_ic_namma_safety_measures", text: getStringBasedOnMode NAMMA_SAFETY_MEASURES isSafetyPlus }
  1 -> { stage: EduNammaSafetyGuidelines, image: "ny_ic_namma_safety_guidlines", text: getStringBasedOnMode SAFETY_GUIDELINES_FOR_YOU isSafetyPlus }
  2 -> { stage: EduNammaSafetyAboutSOS, image: "ny_ic_about_sos_icon", text: getStringBasedOnMode ABOUT_SOS isSafetyPlus }
  _ -> { stage: EduNammaSafetyMeasures, image: "ny_ic_namma_safety_measures", text: "" }

userSettingsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
userSettingsView state push visibility' =
  PrestoAnim.animationSet
    [ Anim.fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility $ boolToVisibility visibility'
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , gravity LEFT
                , padding $ Padding 16 16 16 16
                ]
                [ textView
                    $ [ text $ getString EMERGENCY_ACTIONS
                      , color Color.black900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text $ getStringBasedOnMode WHEN_YOU_START_EMERGENCY_SOS $ isSafetyPlus state
                      , color Color.black700
                      ]
                    <> FontStyle.body3 TypoGraphy
                ]
            , toggleSwitchViewLayout SetDefaultEmergencyContacts state.data.shareToEmergencyContacts (getString EMERGENCY_SHARING_WITH_CONTACTS) push true
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , padding $ Padding 16 8 16 0
                ]
                [ textView
                    $ [ text $ getString SHARING_WITH
                      , color Color.black700
                      , margin $ MarginRight 8
                      , gravity CENTER
                      , visibility $ boolToVisibility $ not $ null state.data.contactsList
                      ]
                    <> FontStyle.body3 TypoGraphy
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    ]
                    (mapWithIndex (\index item -> contactCircleView item index) state.data.contactsList)
                , PrimaryButton.view (push <<< EditEmergencyContacts) (editEmergencyContactsBtnConfig state)
                ]
            , separatorView true
            , toggleSwitchViewLayout SetTriggerCustomerSupport state.data.triggerSupport (getString $ TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE "TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE") push state.data.safetyConfig.enableSupport
            , separatorView state.data.safetyConfig.enableSupport
            , toggleSwitchViewLayout SetNightTimeSafetyAlert state.data.nightSafetyChecks (getString NIGHT_TIME_SAFETY_CHECKS) push true
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< GoBackToActivate) (goBackButtonConfig state) ]
        ]

toggleSwitchViewLayout :: NammaSafetyStage -> Boolean -> String -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchViewLayout stage isActive text' push visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginHorizontal 16 16
    , visibility $ boolToVisibility visibility'
    ]
    [ textView
        $ [ text text'
          , weight 1.0
          , color Color.black800
          ]
        <> FontStyle.body2 TypoGraphy
    , toggleSwitchView isActive stage push
    ]

contactCircleView :: NewContacts -> Int -> forall w. PrestoDOM (Effect Unit) w
contactCircleView contact index =
  linearLayout
    [ height $ V 32
    , width $ V 32
    , background backgroundColor
    , cornerRadius if EHC.os == "IOS" then 12.0 else 20.0
    , gravity CENTER
    , margin $ MarginHorizontal 5 5
    ]
    [ textView
        $ [ text text'
          , color textColor
          ]
        <> FontStyle.tags TypoGraphy
    ]
  where
  backgroundColor = fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0)
  textColor = fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1)
  text' = (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))

-- ---------------------------------- settingUpView -----------------------------------
settingUpView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
settingUpView state push visibility' =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ PrestoAnim.animationSet
        [ Anim.fadeIn true
        ]
        $ settingUpContentView (settingUpContentViewData state) state push
    ]

settingUpContentView :: ContentViewDataType -> NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
settingUpContentView config state push =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginBottom 74
        ]
        [ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderData config.step state.data.safetyConfig.enableSupport)
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 16 32 16 16
            , orientation VERTICAL
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , weight 1.0
                    ]
                    [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET config.image
                        , height $ V 50
                        , margin $ Margin 0 0 14 0
                        , width $ V 50
                        , visibility $ boolToVisibility $ config.image /= ""
                        ]
                    ]
                , toggleSwitchView config.isActive state.props.currentStage push
                ]
            , textView
                $ [ width WRAP_CONTENT
                  , height MATCH_PARENT
                  , text config.title
                  , color Color.black900
                  ]
                <> FontStyle.h2 TypoGraphy
            , textView
                $ [ width WRAP_CONTENT
                  , height MATCH_PARENT
                  , text config.desc
                  , color Color.black700
                  ]
                <> FontStyle.body5 TypoGraphy
            
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding $ PaddingTop 12
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , margin $ MarginHorizontal 8 16
                    , orientation VERTICAL
                    ]
                    ( map
                        ( \item ->
                            measureView (getStringBasedOnMode item isSafetyPlus') true true actionColor 4 FontStyle.Body5
                        )
                        (setupSafetyActionsData state.props.currentStage state.props.enableLocalPoliceSupport isSafetyPlus')
                    )
                , textView
                    $ [ text $ getString PERSONAL_SAFETY_SETTINGS_PERMISSION_REQUEST
                      , color Color.black800
                      , margin $ MarginLeft 24
                      , visibility showPermissionRequest
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
            , scrollView
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , visibility $ boolToVisibility $ state.props.currentStage == SetDefaultEmergencyContacts
                    ]
                    (mapWithIndex (\index item -> contactCardView push state item index) state.data.contactsList)
                ]
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 16 16 16 16
                , visibility $ boolToVisibility $ state.props.currentStage == SetDefaultEmergencyContacts && length state.data.contactsList /= 3
                , cornerRadius 8.0
                , margin $ MarginTop 12
                , onClick push $ const $ AddContacts
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_add_filled"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 12
                    ]
                , textView
                    $ [ text $ getString ADD_A_CONTACT
                      , color Color.blue900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                ]
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , alignParentBottom "true,-1"
        ]
        [ PrimaryButton.view (push <<< GoToNextStep) (continueNextStepButtonConfig state) ]
    ]
    where actionColor = if state.props.currentStage == SetPersonalSafetySettings then Color.black800 else Color.black700
          isSafetyPlus' = isSafetyPlus state
          showPermissionRequest = boolToVisibility $ state.props.currentStage == SetPersonalSafetySettings && isSafetyPlus'

stepsHeaderData :: Int -> Boolean -> StepsHeaderModelState
stepsHeaderData currentIndex supportEnabled =
  { activeIndex: currentIndex
  , textArray: [ getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS ] <> (if supportEnabled then [ getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS ] else [])
  , backArrowVisibility: true
  , config: DC.config
  }

type ContentViewDataType
  = { title :: String
    , desc :: String
    , image :: String
    , step :: Int
    , isActive :: Boolean
    }

settingUpContentViewData :: NammaSafetyScreenState -> ContentViewDataType
settingUpContentViewData state = case state.props.currentStage of
  SetDefaultEmergencyContacts -> { title: getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_TITLE, desc: getStringBasedOnMode SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC isSafetyPlus', image: "ny_ic_share", step: 0, isActive: state.data.shareToEmergencyContacts && length state.data.contactsList /= 0 }
  SetTriggerCustomerSupport -> { title: getString $ TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE "TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE", desc: getString TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_DESC, image: "ny_ic_ny_support", step: 1, isActive: state.data.triggerSupport }
  SetNightTimeSafetyAlert -> { title: getString ENABLE_NIGHT_TIME_SAFETY_ALERTS_TITLE, desc: getStringBasedOnMode ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC isSafetyPlus', image: "ny_ic_night_safety", step: getStepNumber 2, isActive: state.data.nightSafetyChecks }
  SetPersonalSafetySettings -> { title: getString ALMOST_DONE_TITLE, desc: getStringBasedOnMode ALMOST_DONE_DESC isSafetyPlus', image: "", step: getStepNumber 3, isActive: state.data.triggerSupport }
  _ -> { title: "", desc: "", image: "", step: 5, isActive: false }
  where
  getStepNumber step = if state.data.safetyConfig.enableSupport then step else step - 1
  isSafetyPlus' = isSafetyPlus state

setupSafetyActionsData :: NammaSafetyStage -> Boolean -> Boolean -> Array STR
setupSafetyActionsData stage isLocalPoliceSupportEnabled isSafetyPlus =
  case stage, isLocalPoliceSupportEnabled, isSafetyPlus of
    SetPersonalSafetySettings, false, true -> [ PERSONAL_SAFETY_ACTION_1, PERSONAL_SAFETY_ACTION_2 "PERSONAL_SAFETY_ACTION_2", PERSONAL_SAFETY_ACTION_3 ]
    SetPersonalSafetySettings, true, true -> [ PERSONAL_SAFETY_ACTION_1, PERSONAL_SAFETY_ACTION_2_POLICE, PERSONAL_SAFETY_ACTION_3 ]
    SetNightTimeSafetyAlert, _, _ -> [ NIGHT_SAFETY_DESC ]
    _, _, _ -> []

------------------- separator -------------------
separatorView :: forall w. Boolean -> PrestoDOM (Effect Unit) w
separatorView visibility' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , background Color.lightGreyShade
    , visibility $ boolToVisibility visibility'
    ]
    []

educationView :: NammaSafetyScreenState -> Boolean -> forall w. PrestoDOM (Effect Unit) w
educationView state visibility' =
  PrestoAnim.animationSet
    [ Anim.fadeIn true
    ]
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , visibility $ boolToVisibility visibility'
        , padding getSafePadding
        ]
        [ scrollView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                ]
                [ nammaSafetyMeasuresView state (state.props.currentStage == EduNammaSafetyMeasures)
                , safetyGuidelinesView state (state.props.currentStage == EduNammaSafetyGuidelines)
                , aboutSOSView state (state.props.currentStage == EduNammaSafetyAboutSOS)
                ]
            ]
        ]

nammaSafetyMeasuresView :: NammaSafetyScreenState -> Boolean -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyMeasuresView state visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ height $ V 210
        , width MATCH_PARENT
        , cornerRadius 20.0
        , margin $ Margin 16 16 16 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_measures"
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                measureView (getStringBasedOnMode item $ isSafetyPlus state) false true Color.black800 16 FontStyle.Body1
            )
            safetyMeasuresData
        )
    ]

measureView :: String -> Boolean -> Boolean -> String -> Int -> FontStyle.Style -> forall w. PrestoDOM (Effect Unit) w
measureView text' showBullet isCorrect color' marginBottom style =
  linearLayout
    [ height WRAP_CONTENT
    , width $ WRAP_CONTENT
    , margin $ MarginBottom marginBottom
    , gravity LEFT
    ]
    [ textView
        $ [ text "•"
          , visibility $ boolToVisibility showBullet
          , gravity TOP_VERTICAL
          , height MATCH_PARENT
          , margin $ MarginRight 6
          , color color'
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    , imageView
        [ imageWithFallback $ fetchImage FF_ASSET if isCorrect then "ny_ic_tick_green" else "ny_ic_cross"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 16
        , visibility $ boolToVisibility $ not showBullet
        ]
    , textView
        $ [ text text'
          , color color'
          , width WRAP_CONTENT
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    ]

safetyMeasuresData :: Array STR
safetyMeasuresData =
  [ SAFETY_MEASURE_1
  , SAFETY_MEASURE_2
  , SAFETY_MEASURE_3
  , SAFETY_MEASURE_4
  , SAFETY_MEASURE_5 "SAFETY_MEASURE_5"
  ]

safetyGuidelinesView :: NammaSafetyScreenState -> Boolean -> forall w. PrestoDOM (Effect Unit) w
safetyGuidelinesView state visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ height $ V 210
        , width MATCH_PARENT
        , cornerRadius 20.0
        , margin $ Margin 16 16 16 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_guidelines"
        ]
    , textView
        $ [ text $ getString TO_ENSURE_SAFETY_USERS_SHOULD
          , color Color.black800
          , margin $ Margin 16 16 16 0
          ]
        <> FontStyle.body1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 0
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                measureView (getStringBasedOnMode item $ isSafetyPlus state) false true Color.black800 16 FontStyle.Body1
            )
            safetyGuidelinesData
        )
    ]

safetyGuidelinesData :: Array STR
safetyGuidelinesData =
  [ SAFETY_GUIDELINES_1
  , SAFETY_GUIDELINES_2
  , SAFETY_GUIDELINES_3
  , SAFETY_GUIDELINES_4
  , SAFETY_GUIDELINES_5
  , SAFETY_GUIDELINES_6
  , SAFETY_GUIDELINES_7
  ]

aboutSOSView :: NammaSafetyScreenState -> Boolean -> forall w. PrestoDOM (Effect Unit) w
aboutSOSView state visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ height $ V 210
        , width MATCH_PARENT
        , cornerRadius 20.0
        , margin $ Margin 16 16 16 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_about_sos"
        ]
    , textView
        $ [ textFromHtml $ getStringBasedOnMode ABOUT_SOS_DESC $ isSafetyPlus state
          , color Color.black800
          , margin $ Margin 16 16 16 0
          ]
        <> FontStyle.body1 TypoGraphy
    , textView
        $ [ text $ getString FEW_EXAMPLES_OF_SOS_SITUATIONS
          , color Color.black800
          , margin $ Margin 16 16 16 0
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                measureView (getString item.text) false item.isCorrect Color.black800 16 FontStyle.Body1
            )
            aboutSOSDataPoints
        )
    , textView
        $ [ text $ getString THINGS_TO_DO_DURING_SOS_SITUATION
          , color Color.black800
          , margin $ Margin 16 8 16 0
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                measureView (getStringBasedOnMode item $ isSafetyPlus state) true true Color.black800 16 FontStyle.Body1
            )
            aboutSoSData
        )
    ]

aboutSOSDataPoints :: Array { text :: STR, isCorrect :: Boolean }
aboutSOSDataPoints =
  [ { text: ABOUT_SOS_1, isCorrect: true }
  , { text: ABOUT_SOS_2, isCorrect: true }
  , { text: ABOUT_SOS_3, isCorrect: true }
  , { text: ABOUT_SOS_4, isCorrect: true }
  , { text: ABOUT_SOS_5, isCorrect: false }
  , { text: ABOUT_SOS_6, isCorrect: false }
  , { text: ABOUT_SOS_7, isCorrect: false }
  , { text: ABOUT_SOS_8, isCorrect: false }
  ]

aboutSoSData :: Array STR
aboutSoSData =
  [ ABOUT_SOS_9
  , ABOUT_SOS_10
  , ABOUT_SOS_11 "ABOUT_SOS_11"
  , ABOUT_SOS_12
  , ABOUT_SOS_13
  ]

sosActiveView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
sosActiveView state push visibility' =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black900
    , color $ Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
            [ textView
                $ [ text
                      $ getString
                          if state.props.enableLocalPoliceSupport then
                            SAFETY_PLUS_IS_ACTIVE_NOW
                          else
                            EMERGENCY_REQUEST_SENT
                  , margin $ MarginBottom 8
                  , color $ Color.white900
                  ]
                <> FontStyle.h1 TypoGraphy
            , textView
                $ [ text
                      $ getString
                          if state.props.enableLocalPoliceSupport then
                            SAFETY_PLUS_ACTIVE_DESC
                          else
                            SOS_TRIGGERED_DESC
                  , margin $ MarginBottom 12
                  , color $ Color.white900
                  ]
                <> FontStyle.body1 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , margin $ MarginBottom 12
                , gravity LEFT
                , visibility $ boolToVisibility state.props.enableLocalPoliceSupport
                ]
                [ measureView (getString CALL_AND_ALERT_THE_NEAREST_POLICE_CENTRE) true false Color.white900 2 FontStyle.Body1
                , measureView (getString SEND_A_SILENT_SOS_TO_THE_POLICE) true false Color.white900 2 FontStyle.Body1
                , measureView (getString SEND_A_VIDEO_RECORDING_TO_POLICE) true false Color.white900 2 FontStyle.Body1
                ]
            , imageView
                [ height $ V 250
                , width MATCH_PARENT
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_emergency_sent"
                , margin $ MarginHorizontal 16 16
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , alignParentBottom "true,-1"
            , margin $ Margin 16 16 16 16
            ]
            [ textView
                $ [ text $ getString SOS_ACTIONS
                  , color Color.white900
                  ]
                <> FontStyle.subHeading2 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ MarginVertical 8 8
                , gravity CENTER
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width
                        $ if state.data.safetyConfig.enableSupport || state.props.enableLocalPoliceSupport then
                            V ((EHC.screenWidth unit - 48) / 3)
                          else
                            MATCH_PARENT
                    , orientation VERTICAL
                    , gravity CENTER
                    , padding $ PaddingVertical 9 9
                    , cornerRadius 8.0
                    , background Color.redOpacity20
                    , onClick push $ const $ CallForSupport "police"
                    ]
                    [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                        ]
                    , textView
                        $ [ text $ getString CALL_POLICE
                          , gravity CENTER
                          , color Color.white900
                          , fontStyle $ FontStyle.semiBold LanguageStyle
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width $ V ((EHC.screenWidth unit - 48) / 3)
                    , orientation VERTICAL
                    , gravity CENTER
                    , padding $ PaddingVertical 9 9
                    , cornerRadius 8.0
                    , background Color.redOpacity20
                    , margin $ MarginHorizontal 8 8
                    , visibility $ boolToVisibility state.props.enableLocalPoliceSupport
                    , onClick push $ const $ ShareSilentSos Nothing
                    ]
                    [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_whatsapp_white"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                        ]
                    , textView
                        $ [ text $ getString SEND_SILENT_SOS_TO_POLICE
                          , gravity CENTER
                          , color Color.white900
                          , fontStyle $ FontStyle.semiBold LanguageStyle
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width $ V ((EHC.screenWidth unit - 48) / 3)
                    , orientation VERTICAL
                    , gravity CENTER
                    , margin $ MarginHorizontal 8 8
                    , padding $ PaddingVertical 9 9
                    , background Color.redOpacity20
                    , cornerRadius 8.0
                    , visibility $ boolToVisibility state.data.safetyConfig.enableSupport
                    , onClick push $ const $ CallForSupport "ny_support"
                    ]
                    [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_support_unfilled"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                        ]
                    , textView
                        $ [ text $ getString $ CALL_SUPPORT_FOR_SAFETY "CALL_SUPPORT_FOR_SAFETY"
                          , gravity CENTER
                          , color Color.white900
                          , fontStyle $ FontStyle.semiBold LanguageStyle
                          , cornerRadius 8.0
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width $ V ((EHC.screenWidth unit - 48) / 3)
                    , orientation VERTICAL
                    , gravity CENTER
                    , padding $ PaddingVertical 9 9
                    , cornerRadius 8.0
                    , background Color.redOpacity20
                    , visibility $ boolToVisibility $ isSafetyPlus state
                    , onClick
                        ( \action -> do
                            if EHC.os == "IOS" then do
                              void $ pure $ requestCameraAndMicrophonePermissions unit
                            else
                              void $ pure $ JB.askRequestedPermissionsWithCallback [ "android.permission.CAMERA", "android.permission.RECORD_AUDIO" ] push PermissionsCallback
                            void $ push action
                        )
                        (const NoAction)
                    ]
                    [ imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_video"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                        ]
                    , textView
                        $ [ text $ getString if state.props.enableLocalPoliceSupport then SEND_VIDEO_TO_POLICE else RECORD_VIDEO
                          , gravity CENTER
                          , color Color.white900
                          , fontStyle $ FontStyle.semiBold LanguageStyle
                          , cornerRadius 8.0
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                ]
            , PrimaryButton.view (push <<< MarkRideAsSafe) (cancelSOSBtnConfig state)
            ]
        ]
    ]

getHeaderTitle :: NammaSafetyStage -> Boolean -> String
getHeaderTitle stage isSafetyPlus =
  getStringBasedOnMode
    ( case stage of
        EduNammaSafetyMeasures -> NAMMA_SAFETY_MEASURES
        EduNammaSafetyGuidelines -> SAFETY_GUIDELINES_FOR_YOU
        EduNammaSafetyAboutSOS -> ABOUT_SOS
        NammaSafetyVideoRecord -> EMERGENCY_VIDEO
        _ -> NAMMA_SAFETY
    )
    isSafetyPlus

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
contactCardView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactCardView push state contact index =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 18 18 18 18
    , margin $ Margin 0 5 0 5
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    ]
    [ contactCircleView contact index
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , weight 1.0
          , text contact.name
          , color Color.black800
          ]
        <> FontStyle.subHeading1 LanguageStyle
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString REMOVE)
          , color Color.blue900
          , onClick push (const (RemoveButtonClicked contact))
          ]
        <> FontStyle.body2 TypoGraphy
    ]

removeContactPopUpView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]


emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView [ visibility GONE ]

getSafePadding :: Padding
getSafePadding = Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)

shimmerView :: forall w . ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility if state.props.showShimmer then VISIBLE else GONE
    ]
    [ sfl (V 400) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        ]
        [ sfl (V 80) 130 3 (getValueToLocalStore IS_SOS_ACTIVE == "true")
        , sfl (V 80) 130 1 (getValueToLocalStore IS_SOS_ACTIVE /= "true")
        ]
    ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' = 
  shimmerFrameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop marginTop
  , visibility $ boolToVisibility visibility'
  ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ] ( map 
            (\item ->
                linearLayout
                [ height height'
                , width MATCH_PARENT
                , background Color.greyDark
                , cornerRadius 12.0
                , weight 1.0
                , stroke $ "1," <> Color.grey900
                , margin $ Margin 4 4 4 4
                ][]
            ) (1 .. numberOfBoxes)
          )
  ]

toggleSwitchView :: Boolean -> NammaSafetyStage -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive stage push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const $ ToggleSwitch stage
    , visibility $ boolToVisibility $ stage /= SetPersonalSafetySettings
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]