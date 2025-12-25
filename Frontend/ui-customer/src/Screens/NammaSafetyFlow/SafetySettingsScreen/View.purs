{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetySettingsScreen.View where

import Prelude
import PrestoDOM

import Animation (fadeIn, screenAnimation)
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PopupWithCheckbox.View as PopupWithCheckbox
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, null, (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Engineering.Helpers.Utils as EHU
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (liftFlow)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Pooling (delay)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility, spaceSeparatedPascalCase)
import Presto.Core.Types.Language.Flow (fork, await, doAff, Flow)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyFlow.ComponentConfig (goToDrillButtonConfig, shareTripPopupConfig, startNSOnboardingButtonConfig, pastRideSOSConfirmationPopConfig)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Screens.NammaSafetyFlow.SafetySettingsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..))
import Services.API (RideShareOptions(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState, GlobalState(..))


screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SafetySettingsScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ checkAndStatus push initialState
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SafetySettingsScreen action " action
        let
          _ = spy "SafetySettingsScreen state " state
        eval action state
  }

checkAndStatus :: forall st. (Action -> Effect Unit) -> NammaSafetyScreenState -> Flow GlobalState Unit
checkAndStatus push state = do
  mbRideListResp <- getLastRide state.props.checkPastRide
  eiResponse <- Remote.getEmergencySettings ""
  case eiResponse of
    Right response -> do
      case mbRideListResp of
        Nothing -> pure unit
        Just control -> do
          eiResp <- await control
          liftFlow $ handleLastRide eiResp
      liftFlow $ push $ UpdateEmergencySettings response
      liftFlow $ push $ DisableShimmer
    Left err -> do
      let errMessage = if err.code == 400 
                        then err.response.errorMessage 
                        else getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
      void $ pure $ EHU.showToast errMessage
  EHU.toggleLoader false
  where
  getLastRide checkPastRide =
    if checkPastRide then do
      listControl <- fork $ Remote.rideBookingListWithStatus "1" "0" "COMPLETED" Nothing
      pure $ Just $ listControl
    else
      pure $ Nothing

  handleLastRide eiResp = case eiResp of
    Right resp -> push $ CheckRideListResp resp
    Left err -> pure unit

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background if state.props.isOffUs then Color.black900 else Color.white900
            , padding padding'
            , onBackPressed push $ const BackPressed
            ]
            [ Header.view (push <<< SafetyHeaderAction) headerConfig
            , dashboardView state push
            , HV.shimmerView state
            ]
        , if state.props.showRideShareOptionsPopup then PopupWithCheckbox.view (push <<< ShareTripOptionPopup) $ shareTripPopupConfig state else HV.emptyTextView
        , if state.props.showPastRidePopUp && not state.props.isSafetyCenterDisabled then PopUpModal.view (push <<< PopUpModalAC) $ pastRideSOSConfirmationPopConfig state else HV.emptyTextView
        ]
  where
  padding' =
    if EHC.os == "IOS" then
      (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom))
    else
      (PaddingLeft 0)

  headerConfig = if state.props.isOffUs then 
    (Header.config Language) { showLearnMore = state.data.hasCompletedSafetySetup, showCrossImage = true, useLightColor = true }
    else
      (Header.config Language) { showLearnMore = state.data.hasCompletedSafetySetup }

------------------------------------- dashboardView -----------------------------------
dashboardView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dashboardView state push =
  let isOffUs = state.props.isOffUs
  in
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background if isOffUs then Color.black900 else Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not state.props.showShimmer
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ] if isOffUs then 
          [ nammaSafetyViewOffUs state push
          , userSettingsView state push $ not showFeatures
          ]
          else
          [ nammaSafetyFeaturesView state push showFeatures
          , userSettingsView state push $ not showFeatures
          ]
    ]
  where
  showFeatures = (not state.data.hasCompletedSafetySetup) 

nammaSafetyViewOffUs :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyViewOffUs state push =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ featuresViewOffUs state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.black900
            , alignParentBottom "true,-1"
            ]
            [ callPoliceView state push CALL_POLICE
        ]
        ]


callPoliceView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> STR -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push text' =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , margin $ Margin 12 12 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      , accessibilityHint "Call Police Button"
      ]
        <> [ rippleColor Color.rippleShade, onClick push $ const $ DialPolice ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
        , height $ V 26
        , width $ V 26
        ]
    , textView
        $ [ text $ getString text'
          , gravity CENTER
          , color Color.white900
          , margin $ MarginLeft 6
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

featuresViewOffUs :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
featuresViewOffUs state push =
  let merchant = spaceSeparatedPascalCase $ show (getMerchant FunctionCall) -- Need to check other merchants and update logic
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , layoutGravity "center_vertical"
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.black900
        , gravity CENTER
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        , margin $ Margin 16 20 16 6
        ]
        [ flexBoxLayout
          [
            height MATCH_PARENT
          , width WRAP_CONTENT
          , justifyContent JUSTIFY_START
          , flexDirection COLUMN
          , flexWrap WRAP
          , alignItems ALIGN_STRETCH
          , orientation HORIZONTAL
          , padding (Padding 10 6 10 6)
          ][
            textView
            $ [ text $ getString (ADDITIONAL_FEATURES_ON merchant)
              , singleLine false
              , color Color.white900
              ]
            <> FontStyle.subHeading1 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingHorizontal 16 16
            , margin $ MarginTop 12
            , background Color.black800
            , visibility GONE
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ic_namma_yatri_logo"
                , height $ V 20
                , width $ V 20
                , margin $ MarginRight 8
                ]
            , textView
                $ [ text $ getString GUARANTEED_RIDE
                  , color Color.white900
                  , weight 1.0
                  , height WRAP_CONTENT
                  , singleLine false
                  ]
                <> FontStyle.tags TypoGraphy
            ]
          ]
        , linearLayout 
          [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , cornerRadius 12.0
          , stroke $ "1," <> Color.black700
          , background Color.blackOpacity12
          ]
          [ 
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , cornerRadius 12.0
            ]
            [ imageWithTextView (getString NOTIFY_YOUR_EC) true state
            , imageWithTextView (getString EC_CAN_RESPOND) true state
            , imageWithTextView (getString $ QUICK_SUPPORT merchant) true state
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background Color.black700
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ GoToEducationView
            ]
            [ textView
                $ [ textFromHtml $ "<u>" <> (getString $ LEARN_ABOUT_APP_SAFETY_FEAT merchant) <> "</u>"
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
          ]
        ]
    ]
-- ---------------------------------- nammaSafetyFeaturesView -----------------------------------
nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
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
        , background Color.blue600
        , gravity CENTER
        , orientation VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 20 16 6
        , stroke $ "1," <> Color.blue600
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_shield"
            , width $ V 220
            , height $ V 114
            ]
        , textView
            $ [ text $ getString NAMMA_SAFETY_WILL_ENABLE_ACCESS
              , margin $ Margin 16 20 16 4
              , color Color.black800
              , width MATCH_PARENT
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ imageWithTextView (getString AUTOMATIC_CALL_PLACED_TO_EMERGENCY_CONTACTS) true state
            , imageWithTextView (getString $ EMERGENCY_CONTACTS_CAN_FOLLOW state.props.appName) true state
            , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_POLICE) true state
            , imageWithTextView (getString $ ALERT_SAFETY_TEAM state.props.appName) true state
            , imageWithTextView (getString OPTION_TO_REPORT_A_SAFETY_ISSUE) true state
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background Color.grey900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ GoToEducationView
            ]
            [ textView
                $ [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]

imageWithTextView :: String -> Boolean -> NammaSafetyScreenState -> forall w. PrestoDOM (Effect Unit) w
imageWithTextView text' isActive state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingHorizontal 16 16
    , margin $ MarginTop 12
    , background if state.props.isOffUs then Color.blackOpacity12 else Color.blue600
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET if isActive then "ny_ic_check" else "ny_ic_ellipse_outline_grey"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        ]
    , textView
        $ [ text text'
          , color if state.props.isOffUs then Color.white900 else Color.black800
          , weight 1.0
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

userSettingsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
userSettingsView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
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
            , padding $ PaddingTop 16
            ]
            [ toggleSwitchViewLayout (ToggleSwitch SetDefaultEmergencyContacts) state.data.shareToEmergencyContacts (getString EMERGENCY_SHARING_WITH_CONTACTS) push true 16
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
                      , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
                      ]
                    <> FontStyle.body3 TypoGraphy
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    ]
                    (mapWithIndex (\index item -> ContactCircle.view (ContactCircle.getContactConfig item index false true) (push <<< ContactAction)) state.data.emergencyContactsList)
                , textView
                    $ [ text $ getString if null state.data.emergencyContactsList then ADD_CONTACTS else EDIT
                      , color Color.blue900
                      , margin $ MarginLeft 8
                      , gravity CENTER
                      , onClick push $ const $ EditEmergencyContacts
                      , accessibilityHint $ if null state.data.emergencyContactsList then "Add Contacts Button" else "Edit Button"
                      ]
                    <> FontStyle.body2 TypoGraphy
                ]
            , HV.separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , toggleSwitchViewLayout (ToggleSwitch SetNightTimeSafetyAlert) state.data.nightSafetyChecks (getString NIGHT_TIME_SAFETY_CHECKS) push true 16
            , HV.separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                ]
                [ textView
                    $ [ text shareSettingsText
                      , color Color.black800
                      , padding $ Padding 16 16 0 16
                      , weight 1.0
                      ]
                    <> FontStyle.body1 TypoGraphy
                , textView
                    $ [ text $ getString EDIT
                      , color Color.blue900
                      , margin $ MarginLeft 8
                      , gravity CENTER
                      , padding $ Padding 16 16 16 16
                      , onClick push $ const ShowShareTripOptions
                      ]
                    <> FontStyle.body2 TypoGraphy
                ]
            , textView
                $ [ text $ getString $ WHO_CAN_TRACK_YOUR_RIDE state.props.appName
                  , color Color.black700
                  , margin $ Margin 16 16 16 16
                  , visibility $ boolToVisibility $ (not $ null state.data.emergencyContactsList) && state.data.shareTripWithEmergencyContactOption /= NEVER_SHARE
                  ]
                <> FontStyle.body1 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , visibility $ boolToVisibility $ (not $ null state.data.emergencyContactsList) && state.data.shareTripWithEmergencyContactOption /= NEVER_SHARE
                ]
                ( mapWithIndex
                    ( \index item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , margin $ Margin 16 16 0 0
                          , gravity CENTER_VERTICAL
                          ]
                          [ ContactCircle.view (ContactCircle.getContactConfig item index false true) (push <<< ContactAction)
                          , toggleSwitchViewLayout (ChangeFollowing index) item.enableForFollowing item.name push true 12
                          ]
                    )
                    state.data.emergencyContactsList
                )
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , visibility $ boolToVisibility $ getValueToLocalStore IS_SOS_ACTIVE /= "true" && state.data.shareToEmergencyContacts
            ]
            [ PrimaryButton.view (push <<< StartTestDrill) (goToDrillButtonConfig state) ]
        ]
  where
  shareSettingsText = case state.data.shareTripWithEmergencyContactOption of
    ALWAYS_SHARE -> getString ALWAYS_SHARE_DESC
    SHARE_WITH_TIME_CONSTRAINTS -> getString NIGHT_RIDES_DESC
    NEVER_SHARE -> getString NEVER_SHARE_DESC
  
toggleSwitchViewLayout :: Action -> Boolean -> String -> (Action -> Effect Unit) -> Boolean -> Int -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchViewLayout action isActive text' push visibility' marginLeft =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginHorizontal marginLeft 16
    ]
    [ textView
        $ [ text text'
          , weight 1.0
          , color Color.black800
          ]
        <> FontStyle.body2 TypoGraphy
    , toggleSwitchView isActive true action push
    ]

toggleSwitchView :: Boolean -> Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive visibility' action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action --ToggleSwitch stage
    , visibility $ boolToVisibility visibility'
    , accessibilityHint $ if isActive then "Toggle Button Enabled" else "toggle button disabled"
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]

shimmerView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  let 
    isSosActive = getValueToLocalStore IS_SOS_ACTIVE == "true"
  in
    relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 16
      , visibility $ boolToVisibility state.props.showShimmer
      ]
      [ sfl (V 400) 16 1 true
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , alignParentBottom "true,-1"
          ]
          [ sfl (V 80) 130 3 isSosActive
          , sfl (V 80) 130 1 $ not isSosActive
          ]
      ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 .. numberOfBoxes)
        )
    ]