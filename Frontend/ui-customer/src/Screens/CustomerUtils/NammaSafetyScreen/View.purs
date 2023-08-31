{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.View
  where

import Common.Types.App
import Screens.NammaSafetyScreen.ComponentConfig

import Animation (screenAnimation)
import Animation as Anim
import Animation.Config (translateYAnimConfig)
import Components.GenericHeader as GenericHeader
import Components.NewContact.View as NewContact
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, null, take, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Split (split)
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop, screenWidth)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, setRefreshing, startOtpReciever, storeCallBackContacts)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.DefaultConfig as DC
import Prelude (Unit, bind, const, discard, not, pure, show, unit, when, ($), (&&), (/=), (<<<), (<>), (==), (>=), (||), map, (-))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, editText, fontStyle, frameLayout, getPushFn, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, pattern, relativeLayout, scrollView, singleLine, stroke, swipeRefreshLayout, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.List as PrestoList
import Screens.EmergencyContactsScreen.Controller (contactColorsList)
import Screens.NammaSafetyScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (Contacts, NammaSafetyScreenState, NewContacts, Stage(..), StepsHeaderModelState)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: NammaSafetyScreenState -> PrestoList.ListItem -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState listItemm =
  { initialState
  , view : view listItemm
  , name : "NammaSafetyScreen"
  , globalEvents : [ (\push ->
    do
    --   _ <- JB.setFCMToken push $ SetToken
    --   if not initialState.props.enterOTP then JB.detectPhoneNumbers push $ SetPhoneNumber else pure unit
    --   if initialState.data.timerID == "" then pure unit else pure $ EHC.clearTimer initialState.data.timerID
    --   if not initialState.props.resendEnable && initialState.data.attempts >= 0 && initialState.props.enterOTP then do
    --       _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do 
    --         if (EHC.os == "IOS") then liftEffect $ JB.startTimerWithTime (show initialState.data.timer) "otp" "1" push CountDown
    --         else  liftEffect $ EHC.countDown initialState.data.timer "otp" push CountDown
    --       pure unit
    --     else pure unit
        case initialState.props.currentStage of
            NammaSafetyDashboard -> do
                pure unit
            -- NammaSafetyDashboard -> case initialState.props.onboardingComplete of
            --     true -> do
            --         pure unit
            --     false -> do
            --         pure unit
            SetTriggerCustomerSupport -> do
                pure unit
            SetNightTimeSafetyAlert -> do
                pure unit
            SetDefaultEmergencyContacts -> pure unit
            SetPersonalSafetySettings -> pure unit
            _ -> pure unit
        pure (pure unit)
        ) ] 
  , eval : \action state -> do
      let _ = spy "NammaSafety action " action
      let _ = spy "NammaSafety state " state
      eval action state
  }

listItem1 :: NewContacts
listItem1 = {
  name: "",
  number: "",
  isSelected: false
}

view
  :: forall w. PrestoList.ListItem ->  (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view listItemm push state = let
  lang = getValueToLocalStore LANGUAGE_KEY
   in
   linearLayout
   [  height MATCH_PARENT
    , width MATCH_PARENT
    , background if any (_ == state.props.currentStage)[ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord] then Color.black900 else Color.white900
   ][  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
        _ <- push action
        -- _ <- requestKeyboardShow (getNewIDWithTag "EnterMobileNumberEditText")
        pure unit
        ) (const AfterRender)
    , margin $ MarginBottom if state.props.currentStage == NammaSafetyDashboard && state.props.showOnboarding == false then 0 else 24
    , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
    , onBackPressed push (const BackPressed )
    ][  
        headerView state push
        , if state.props.currentStage == NammaSafetyDashboard then dashboardView state push 
        else if state.props.currentStage == EmergencyContactsStage then emergencyContactsView listItemm push state
        else if state.props.currentStage == AboutNammaSafety then aboutNammaSafetyView state push
        else if state.props.currentStage == SetTriggerCustomerSupport || state.props.currentStage == SetNightTimeSafetyAlert || state.props.currentStage == SetDefaultEmergencyContacts || state.props.currentStage == SetPersonalSafetySettings then settingUpView state push 
        else if state.props.currentStage == EduNammaSafetyMeasures || state.props.currentStage == EduNammaSafetyGuidelines || state.props.currentStage == EduNammaSafetyAboutSOS  then educationView state push 
        else if state.props.currentStage == ActivateNammaSafety then activateNammaSafetyView state push
        else if state.props.currentStage == TriggeredNammaSafety then sosActiveView state push
        else if state.props.currentStage == NammaSafetyVideoRecord then videoRecordSOSView state push
        else linearLayout[][]
        -- chooseActionSOSView state push
        -- videoRecordSOSView state push
        -- sosActiveView state push
    --     PrestoAnim.animationSet
    --       [ Anim.fadeIn true
    --       ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (StepsHeaderModel.stepsHeaderData 0)
    --   , frameLayout
    --     [ width MATCH_PARENT
    --     , height MATCH_PARENT
    --     , padding (Padding 16 0 16 0)
    --     ][
    --       PrestoAnim.animationSet
    --         [ Anim.fadeOut true
    --         ]
      ]
    ]

-- ---------------------------------- dashboardView -----------------------------------

dashboardView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
dashboardView state push=
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
--   , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
      linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , if state.data.hasCompletedSafetySetup == false then nammaSafetyFeaturesView state push else userSettingsView state push
      ]
  ]

headerView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerView state push = 
    linearLayout [
        height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility if any (_ == state.props.currentStage)[SetTriggerCustomerSupport, SetNightTimeSafetyAlert, SetDefaultEmergencyContacts, SetPersonalSafetySettings] then GONE else VISIBLE
    ][
        linearLayout[
            height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
        ][
            linearLayout[
                height WRAP_CONTENT
                , width WRAP_CONTENT
                , weight 1.0
            ][
                GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig (getHeaderTitle state.props.currentStage) state)
            ]
            , textView [
                text "Learn More"
                , visibility if (state.props.currentStage == NammaSafetyDashboard && state.data.hasCompletedSafetySetup == true || state.props.currentStage == ActivateNammaSafety) then VISIBLE else GONE
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
        ][]
    ]

aboutNammaSafetyView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
aboutNammaSafetyView state push =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  textView 
            [ text "Learn about Namma Safety"
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black700
            , background Color.blue600
            , gravity LEFT
            , padding $ Padding 12 16 12 16] 
            , cardView state (getCardViewData 0) push
            , cardView state (getCardViewData 1) push
            , cardView state (getCardViewData 2) push
        ]
        , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , background Color.white900
              , alignParentBottom "true,-1"
              ][  PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
    ]

nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push = 
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ featuresView state push
        , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , background Color.white900
              , alignParentBottom "true,-1"
              ][  PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
    ]

featuresView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
featuresView state push = 
    linearLayout[
        width MATCH_PARENT
        , height WRAP_CONTENT
        , background if state.props.currentStage == ActivateNammaSafety then "#373A45" else Color.blue600
        , gravity CENTER
        , orientation VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 20 16 0
        , stroke $ if state.props.currentStage == ActivateNammaSafety then "1," <> Color.black700 else "1," <> Color.blue600
    ][
        imageView [
            imageWithFallback "ny_ic_safety_shield, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png"
            , width $ V 220
            , height $ V 114
        ]
        , textView $ [
                text "Namma Safety will enable access to the following features during a ride!"
                , margin $ Margin 16 20 16 20
                , color if state.props.currentStage == ActivateNammaSafety then Color.white900 else Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
        , imageWithTextView "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" "Share location and ride details with Namma Yatri Support Team" true state.props.currentStage
        , imageWithTextView "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" "Activate live video recording features" true state.props.currentStage
        , imageWithTextView "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" "Get options to directly call support / Police" true state.props.currentStage
        , imageWithTextView "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" "Share location and ride details with your emergency contacts" true state.props.currentStage
        , linearLayout [
            height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 0 16 16
            , background Color.white900
        ][]
        , linearLayout [][
            textView $ [
            textFromHtml if state.props.currentStage == ActivateNammaSafety then "<u>Edit Actions</u>" else "<u>Learn More</u>"
            , color Color.blue800
            , margin $ MarginBottom 20
            , onClick push $ const $ SwitchToStage $ if state.props.currentStage == ActivateNammaSafety then NammaSafetyDashboard else AboutNammaSafety
        ] <> FontStyle.body1 TypoGraphy
        ]
    ]

imageWithTextView :: String -> String -> Boolean -> Stage -> forall w . PrestoDOM (Effect Unit) w
imageWithTextView image text' isActive stage = 
        linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding $ PaddingHorizontal 16 16
            , margin $ MarginVertical 0 16
        ][
            imageView [
                imageWithFallback if isActive then "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" else "ny_ic_ellipse_outline_grey, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png"
                , height $ V 20
                , width $ V 20
                , margin $ MarginRight 8
            ]
            , textView $ [
                text text'
                , color if stage == ActivateNammaSafety then Color.white900 else Color.black800
            ] <> FontStyle.tags TypoGraphy
        ]

activateNammaSafetyView :: NammaSafetyScreenState  -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
activateNammaSafetyView state push = 
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ featuresView state push
        , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , alignParentBottom "true,-1"
              ][  PrimaryButton.view (push <<< ActivateSOS) (activateSoSButtonConfig state),
                PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (dismissSoSButtonConfig state) ]
    ]

cardView:: NammaSafetyScreenState  -> CardViewDataType -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardView state cardData push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding (Padding 16 16 16 16)
    , margin (Margin 16 16 16 0)
    , stroke ("1,"<>Color.grey900)
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , onClick push $ const $ SwitchToStage cardData.stage
    ][ 
            imageView
            [ imageWithFallback cardData.image
            , height $ V 60
            , margin (Margin 0 0 14 0)
            , width $ V 60
            ]
            , textView [
                width WRAP_CONTENT
                , height MATCH_PARENT
                , text cardData.text
                , gravity CENTER_VERTICAL
                , color Color.black800
                , textSize FontSize.a_14
                , fontStyle $ FontStyle.bold LanguageStyle
            ]
    ]

type CardViewDataType = {
    stage :: Stage,
    image :: String,
    text :: String
}

getCardViewData :: Int -> CardViewDataType
getCardViewData index = case index of
    0 -> {stage : EduNammaSafetyMeasures , image : "ny_ic_circle, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : "Namma Safety Measures"}
    1 -> {stage : EduNammaSafetyGuidelines, image : "ny_ic_circle, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : "Safety Guidelines for you"}
    2 -> {stage : EduNammaSafetyAboutSOS, image : "ny_ic_circle, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : "About SOS"}
    _ -> {stage : EduNammaSafetyMeasures , image : "ny_ic_circle, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : "Namma Safety Measures"}


userSettingsView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
userSettingsView state push=
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        
        ][ 
            linearLayout [
            width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL, gravity LEFT
            , padding $ Padding 16 16 16 16]
            [
                textView [ text "Emergency Actions"
                , color Color.black900
                , textSize FontSize.a_16
                , fontStyle $ FontStyle.bold LanguageStyle 
                ] 
                , textView [ text "When you start Emergency SOS, your app will perform the below actions"
                , color Color.black700
                , textSize FontSize.a_12
                , fontStyle $ FontStyle.semiBold LanguageStyle ] 
                
            ]
            , toggleSwitchViewLayout SetDefaultEmergencyContacts state.data.shareToEmergencyContacts "Emergency Sharing with contacts" push
            , linearLayout [
                width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , padding $ Padding 16 16 16 16

            ]
            [
                textView [ text "Sharing With:"
                , height MATCH_PARENT
                , color Color.black700
                , textSize FontSize.a_12
                , margin $ MarginRight 8   
                , gravity CENTER
                , fontStyle $ FontStyle.medium LanguageStyle ] 
                , linearLayout [](mapWithIndex (\index item -> contactCircleView state push item index) state.data.emergencyContactsData.contactsList)
                , PrimaryButton.view (push <<< EditEmergencyContacts) (editEmergencyContactsBtnConfig state)
                
            ]
            , separatorView
            , toggleSwitchViewLayout SetTriggerCustomerSupport state.data.triggerNYSupport "Trigger alert to NammaYatri support" push
            , separatorView
            , toggleSwitchViewLayout SetNightTimeSafetyAlert state.data.nightTimeSafety "Night Time Safety Checks" push
        ]
    ]

toggleSwitchViewLayout ::  Stage -> Boolean -> String -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchViewLayout stage isActive text' push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 16 16 16 16
  ][  
    textView [ 
        text text'
        , weight 1.0
        , color Color.black800
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle ]
    , toggleSwitchView isActive stage push
  ]

contactCircleView ::  NammaSafetyScreenState -> (Action -> Effect Unit) -> NewContacts -> Int -> forall w. PrestoDOM (Effect Unit) w
contactCircleView state push contact index =
  linearLayout
    [ height $ V 32
    , width $ V 32
    , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
    , cornerRadius if EHC.os == "IOS" then 12.0 else 20.0
    , gravity CENTER
    , margin (MarginHorizontal 5 5)
    ]
    [ textView
        [ text (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))
        , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
        , textSize FontSize.a_12
        ]
    ]

getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (DS.split (DS.Pattern " ") (fullName)))

getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))
  
-- ---------------------------------- settingUpView -----------------------------------
settingUpView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
settingUpView state push=
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  PrestoAnim.animationSet
        [ Anim.fadeIn true
        ] $ settingUpContentView (settingUpContentViewData state) state push
        , if (state.props.emergencyContactsProps.showInfoPopUp == true) then removeContactPopUpView push state else emptyTextView state
    
  ]

settingUpContentView :: ContentViewDataType -> NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
settingUpContentView config state push = 
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ linearLayout [
        height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
    ][
        StepsHeaderModel.view (push <<< StepsHeaderModelAC)  (stepsHeaderData config.step)
        , linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , padding (Padding 16 32 16 16)
            , orientation VERTICAL
        ][
            linearLayout [
                height WRAP_CONTENT
                , width MATCH_PARENT
            ][
                linearLayout [
                    weight 1.0
                ][
                    imageView
                        [ imageWithFallback "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png"
                        , height $ V 50
                        , margin (Margin 0 0 14 0)
                        , width $ V 50
                        , visibility if config.image /= "" then VISIBLE else GONE
                        ]
                ]
                , toggleSwitchView config.isActive state.props.currentStage push
            ]
            , textView [
                width WRAP_CONTENT
                , height MATCH_PARENT
                , text config.title
                -- , gravity CENTER_VERTICAL
                , color Color.black900
                , textSize FontSize.a_24
                , fontStyle $ FontStyle.bold LanguageStyle
            ]
            , textView [
                width WRAP_CONTENT
                , height MATCH_PARENT
                , text config.desc
                -- , gravity CENTER_VERTICAL
                , color Color.black700
                , textSize FontSize.a_16
                , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
            , linearLayout [
                width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , visibility if state.props.currentStage == SetDefaultEmergencyContacts then VISIBLE else GONE
                , afterRender
                    ( \action -> do
                        _ <- push action
                        _ <- storeCallBackContacts push ContactsCallback
                        if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
                            _ <- push FetchContacts
                            pure unit
                        else do
                            pure unit
                        _ <- push CheckingContactList
                        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag") false
                        pure unit
                    )
                    (const NoAction)
            ](mapWithIndex (\index item -> contactCardView push state item index) state.data.emergencyContactsData.contactsList)
            , linearLayout [
                width MATCH_PARENT
                , height WRAP_CONTENT
                , stroke $ "1,"<> Color.grey900
                , padding $ Padding 16 16 16 16
                , visibility if state.props.currentStage == SetDefaultEmergencyContacts && length state.data.emergencyContactsData.contactsList /= 3 then VISIBLE else GONE
                , cornerRadius 8.0
                , margin $ MarginTop 12
                , onClick push $ const $ AddContacts
            ][
                imageView [
                    imageWithFallback "ny_ic_add_filled,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 12
                ]
                , textView $ [
                    text "Add a contact"
                    , color Color.blue900
                ] <> FontStyle.subHeading1 TypoGraphy
            ]

        ]
    ]
    , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , alignParentBottom "true,-1"
    ][  PrimaryButton.view (push <<< GoToNextStep) (continueNextStepButtonConfig state)
        -- , if state.props.currentStage /= SetPersonalSafetySettings then PrimaryButton.view (push <<< SkipToNextStep) (skipNSOnboardingButtonConfig state) else linearLayout [][] 
        ]
    ]

stepsHeaderData :: Int -> StepsHeaderModelState
stepsHeaderData currentIndex = 
    {
        activeIndex : currentIndex,
        textArray : ["Set up your personal safety settings", "Set up your personal safety settings", "Set up your personal safety settings", "Set up your personal safety settings"],
        backArrowVisibility : true,
        config : DC.config
    }

type ContentViewDataType = {
  title :: String,
  desc :: String,
  image :: String,
  step :: Int,
  isActive :: Boolean
}

settingUpContentViewData :: NammaSafetyScreenState -> ContentViewDataType
settingUpContentViewData state = case state.props.currentStage of
    SetDefaultEmergencyContacts ->  {title: "Share Info with emergency contacts?", desc: "On SOS, the ride information will be auto shared with the below emergency contacts.", image: "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 0, isActive : state.data.shareToEmergencyContacts && length state.data.emergencyContactsData.contactsList /= 0}
    SetTriggerCustomerSupport ->  {title: "Trigger alert to NammaYatri support", desc: "We have 24*7 dedicated support who will be alerted automatically", image: "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 1, isActive : state.data.triggerNYSupport}
    SetNightTimeSafetyAlert ->  {title: "Enable night time safety alerts?", desc: "To ensure your safety, from 9 PM-6AM, \nwe would send safety check alerts basis anomaly detection. In cases of route-deviation or when vehicle is not moving", image: "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 2, isActive : state.data.nightTimeSafety}
    SetPersonalSafetySettings ->  {title: "Almost Done!", desc: "During SOS, based on criticality of situation, any of these options can be selected\n\n \t• Call 112\n \t• Call NammaYatri support\n \t• Record video", image: "", step : 3, isActive : state.data.triggerNYSupport}
    _ -> {title:"", desc:"", image:"", step : 5, isActive : false}

------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height (V 1)
  , width MATCH_PARENT
  , margin (Margin 16 16 16 16)
  , background Color.lightGreyShade
  ][]

educationView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
educationView state push=
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
        if state.props.currentStage == EduNammaSafetyMeasures then nammaSafetyMeasuresView state
          else if state.props.currentStage == EduNammaSafetyGuidelines then safetyGuidelinesView state
          else aboutSOSView state
        ]
   ]    

nammaSafetyMeasuresView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
nammaSafetyMeasuresView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        linearLayout
            [ height $ V 210
            , width MATCH_PARENT
            , background Color.greySmoke
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            ][]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item false true
                ) safetyMeasuresData) 
    ]

measureView :: String -> Boolean -> Boolean -> forall w . PrestoDOM (Effect Unit) w
measureView text' showBullet isCorrect = 
    linearLayout [
        margin $ MarginBottom 16
        , gravity CENTER
    ][
        textView $ [
            text "•"
            , visibility if showBullet then VISIBLE else GONE
            , gravity TOP_VERTICAL
            , height MATCH_PARENT
            , margin $ MarginRight 5
        ] <> FontStyle.body1 TypoGraphy
        , imageView [
            imageWithFallback if isCorrect then "ny_ic_tick,user/nammaYatri/res/drawable/ny_ic_tick.png" else "ny_ic_cross,user/nammaYatri/res/drawable/ny_ic_cross.png"
            , height $ V 20
            , width $ V 20
            , margin $ MarginRight 16
            , visibility if showBullet then GONE else VISIBLE
        ]
        , textView $ [
            text text'
        ] <> FontStyle.body1 TypoGraphy
    ]

safetyMeasuresData :: Array String
safetyMeasuresData = [
    "Night-time safety check based on ride anomaly detection",
    "24/7 dedicated NammaSafety support",
    "SOS Namma Safety button.",
    "Zero tolerance policy for drivers/customers",
    "Customer privacy - drivers can't view the exact address of the ride once completed",
    "Safety Training and Certification for all Namma Yatri Drivers"
    ]

safetyGuidelinesView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
safetyGuidelinesView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        linearLayout
            [ height $ V 210
            , width MATCH_PARENT
            , background Color.greySmoke
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            ][]
        , textView $ [
            text "To ensure safety, users should:"
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.body1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item false true
                ) safetyGuidelinesData) 
    ]

safetyGuidelinesData :: Array String
safetyGuidelinesData = [
    "Check Driver’s identification with vehicle number",
    "Keep an eye on the route",
    "Stay alert especially during night rides",
    "If you feel uncomfortable, share your ride with emergency contacts",
    "Avoid sharing personal information",
    "Use In-App emergency Namma Safety feature",
    "Rate and Review to help maintain a reliable and safe community"
    ]

aboutSOSView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
aboutSOSView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        linearLayout
            [ height $ V 210
            , width MATCH_PARENT
            , background Color.greySmoke
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            ][]
        , textView $ [
            textFromHtml "Kindly understand the criticality of SOS. For non-critical situations, kindly use the <b>Help & Support</b> section to raise a complaint."
            , color Color.black800
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.body1 TypoGraphy
        , textView $ [
            text "Few examples of SOS situations"
            , color Color.black800
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item.text false item.isCorrect
                ) aboutSOSDataPoints) 
        , textView $ [
            text "Things to do during SOS situation"
            , color Color.black800
            , margin $ Margin 16 8 16 0
        ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ] (map (\item -> 
                    measureView item true true
                ) aboutSoSData) 
    ]

aboutSOSDataPoints :: Array {text :: String, isCorrect :: Boolean}
aboutSOSDataPoints = [
    {text : "Driver re-routing to different remote locations", isCorrect : true},
    {text : "Inappropriate verbal/physical cues from the drive", isCorrect : true},
    {text : "Other male passengers asked to join ride without your consent", isCorrect : true},
    {text : "Driver stopped unsolicited, at a remote area", isCorrect : true},
    {text : "Fare issues", isCorrect : false},
    {text : "Drunk driving", isCorrect : false},
    {text : "Rash driving", isCorrect : false},
    {text : "Vehicle number mismatch", isCorrect : false}
    ]

aboutSoSData :: Array String
aboutSoSData = [
    "Activate SOS by clicking on Namma Safety button during the ride.",
    "Remain Calm. Do not panic.",
    "Inform your family & friends/ Namma Yatri support by emergency sharing.",
    "Choose an appropriate SOS option from: Call Police (112)/ Call Namma Yatri support/ Record video (to share with emergency contacts & Namma Yatri",
    "If possible, ask the driver to stop the vehicle at a nearby populated area"
]

sosActiveView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
sosActiveView state push = 
 Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black900
  , color $ Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ][
            textView [
                text "Emergency request Sent!"
                    , textSize FontSize.a_22
                    , margin $ MarginBottom 8
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color $ Color.white900
            ]
            , textView $ [
                text "Please stay calm, Your real time location is being shared with your emergency contacts and our support team"
                    , margin $ MarginBottom 12
                    , color $ Color.white900
            ] <> FontStyle.body1 TypoGraphy
            , imageView [
                height $ V 280
                , width MATCH_PARENT
                , imageWithFallback "ny_ic_emergency_sent,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                , margin $ MarginHorizontal 16 16
              ]
            , textView $ [
                text "SOS Actions"
                , color Color.white900
            ] <> FontStyle.subHeading2 TypoGraphy
            , linearLayout[
                width MATCH_PARENT
                , margin $ MarginTop 8
                , gravity CENTER
                , orientation HORIZONTAL
            ][
                linearLayout[
                orientation VERTICAL
                , gravity CENTER
                , padding $ PaddingVertical 9 9
                , cornerRadius 8.0
                , background "#373A45"
                , weight 1.0
                , onClick push $ const $ CallForSupport "police"
                ][
                    imageView [
                        imageWithFallback "ny_ic_police,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                    ]
                    , textView $ [
                        textFromHtml "Call Police<br>(112)"
                        , gravity CENTER
                        , color Color.white900
                        , fontStyle $ FontStyle.semiBold LanguageStyle
                    ] <> FontStyle.paragraphText TypoGraphy
                ]
                , linearLayout[
                    orientation VERTICAL
                    , gravity CENTER
                    , margin $ MarginHorizontal 8 8
                    , padding $ PaddingVertical 9 9
                    , background "#373A45"
                    , cornerRadius 8.0
                    , weight 1.0
                    , onClick push $ const $ CallForSupport "ny_support"
                ][
                    imageView [
                        imageWithFallback "ny_ic_support_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                    ]
                    , textView $ [
                        textFromHtml "Call Our<br>Support"
                        , gravity CENTER
                        , color Color.white900
                        , fontStyle $ FontStyle.semiBold LanguageStyle
                        , cornerRadius 8.0
                    ] <> FontStyle.paragraphText TypoGraphy
                ]
                , linearLayout[
                    orientation VERTICAL
                    , gravity CENTER
                    , padding $ PaddingVertical 9 9
                    , cornerRadius 8.0
                    , background "#373A45"
                    , weight 1.0
                    , onClick push $ const $ SwitchToStage NammaSafetyVideoRecord
                ][
                    imageView [
                        imageWithFallback "ny_ic_video,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 26
                        , width $ V 26
                        , margin $ MarginBottom 8
                    ]
                    , textView $ [
                        textFromHtml "Record<br>Video"
                        , gravity CENTER
                        , color Color.white900
                        , fontStyle $ FontStyle.semiBold LanguageStyle
                        , cornerRadius 8.0
                    ] <> FontStyle.paragraphText TypoGraphy
                ]
            ]
            , PrimaryButton.view (push <<< GoToNextStep) (cancelSOSBtnConfig state)
        ]
      ]
  ]

videoRecordSOSView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
videoRecordSOSView state push = 
 Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black900
  , color $ Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  
    linearLayout[
        height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
    ][
        linearLayout [
            height $ V 300
            , width MATCH_PARENT
            , background $ Color.red
            , margin $ MarginTop 37
            , id $ EHC.getNewIDWithTag "VideoCamView"
            -- , gravity CENTER
        ][]
        , textView $ [
            text "The video will be recorded for 15 sec only and will be shared as per your settings"
            , color "#B9BABE"
        ] <> FontStyle.body3 TypoGraphy
    ]
    , linearLayout[
        width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , alignParentBottom "true,-1"
    ][
        imageView [
            imageWithFallback "ny_ic_start_record,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
            , height $ V 45
            , width $ V 45
        ]
        , textView $ [
            text "Start Recording"
            , color Color.white900
        ] <> FontStyle.tags TypoGraphy
    ]
]

toggleSwitchView :: Boolean -> Stage -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchView isActive stage push = 
    linearLayout[
        height MATCH_PARENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , onClick push $ const $ ToggleSwitch stage 
    ][
        imageView [
            imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
            , width $ V 40
            , height $ V 24
        ]  
    ]

getHeaderTitle :: Stage -> String
getHeaderTitle stage = 
    case stage of 
        EduNammaSafetyMeasures   -> "Namma Safety Measures"
        EduNammaSafetyGuidelines -> "Safety Guidelines for you"
        EduNammaSafetyAboutSOS   -> "About SOS"
        NammaSafetyVideoRecord   -> "Emergency Video"
        _                        -> getString NAMMA_SAFETY


emergencyContactsView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView listItemm push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 (if safeMarginBottom == 0 && os == "IOS" then 16 else safeMarginBottom)) else (Padding 0 0 0 0)
        , gravity CENTER
        , afterRender
            ( \action -> do
                _ <- push action
                _ <- storeCallBackContacts push ContactsCallback
                if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
                  _ <- push FetchContacts
                  pure unit
                else do
                  pure unit
                _ <- push CheckingContactList
                _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag") false
                pure unit
            )
            (const NoAction)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ 
            --     GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
            -- , 
            linearLayout
                [ height $ V 1
                , width $ V (screenWidth unit)
                , background Color.greySmoke
                ]
                []
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , gravity CENTER
                , padding (Padding 16 0 16 0)
                , visibility if state.props.emergencyContactsProps.showContactList then GONE else VISIBLE
                ]
                [ emergencyContactsSelectView push state
                , PrimaryButton.view (push <<< AddEmergencyContacts) (addContactButtonConfig state)
                ]
            ]
        , if state.props.emergencyContactsProps.showContactList then (contactListView listItemm push state) else emptyTextView state
        ]
------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

------------------------ ContactsListView ---------------------------
contactListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
contactListView listItemm push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ 
    --     linearLayout
    --     [ height WRAP_CONTENT
    --     , width MATCH_PARENT
    --     , orientation VERTICAL
    --     , background Color.white900
    --     ]
    --     [ GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
    --     , horizontalLine
    --     ]
    -- , 
    linearLayout
        [ width MATCH_PARENT
        , height $ V 44
        , orientation HORIZONTAL
        , cornerRadius 8.0
        , padding (Padding 2 2 2 2)
        , margin (Margin 16 16 16 16)
        , gravity LEFT
        , stroke ("1," <> Color.borderColorLight)
        ]
        [ editText
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , weight 1.0
            , textSize FontSize.a_16
            , padding (Padding 14 10 0 10)
            , color Color.black800
            , gravity LEFT
            , id (getNewIDWithTag "contactEditText")
            , background Color.white900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , text ""
            , hint $ getString SEARCH_CONTACTS
            , pattern "[^\n]*,255"
            , onChange push $ ContactTextChanged
            ]
        , imageView
            [ height $ V 17
            , width $ V 17
            , imageWithFallback "ny_ic_cancel,https://assets.juspay.in/nammayatri/images/user/ny_ic_cancel.png"
            , gravity CENTER
            , margin (Margin 10 10 10 10)
            , onClick push $ const ContactListClearText
            ]
        ]
    , showEmergencyContact listItemm push state
    , linearLayout
        [ height if os == "IOS" then (V 84) else WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (Padding 16 16 16 24)
        , stroke $ "1," <> Color.grey900
        , alignParentBottom "true,-1"
        , margin (Margin 0 0 0 0)
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height if os == "IOS" then (V 52) else WRAP_CONTENT
            , gravity BOTTOM
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< ContactListPrimaryButtonActionController) (contactListPrimaryButtonConfig state.data.emergencyContactsData.contactsCount)
            ]
        ]
    ]

showEmergencyContact :: forall w. PrestoList.ListItem ->  (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContact listitemm push config =
  swipeRefreshLayout
    ([ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    , onRefresh push (const RefreshScreen)
    ] <> if os == "IOS" then [] else [id $ getNewIDWithTag "EmergencyContactTag"] )
    [ showEmergencyContactData listitemm push config
    ]

showEmergencyContactData :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContactData listItemm push state =
  Keyed.linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]
  [ Tuple "contacts"
    $ PrestoList.list
    [ height MATCH_PARENT
    -- , scrollBarY false
    , width MATCH_PARENT
    , onScroll "contacts" "EmergencyContactsScreen" push (ContactListScroll)
    , onScrollStateChange push (ContactListScrollStateChanged)
    , PrestoList.listItem listItemm
    , background Color.white900
    , PrestoList.listDataV2  $ state.data.emergencyContactsData.prestoListArrayItems
    ]
  ]

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) str == prefix




horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]


--------------------------------------------------- emergencyContactsSelectView -----------------------------------------------------
emergencyContactsSelectView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsSelectView push state = 
  linearLayout
    [ width $ MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ emergencyContactsListView push state
    ]


--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 10 0 10)
    , visibility if (null state.data.emergencyContactsData.contactsList) then GONE else VISIBLE
    , weight 1.0
    ]
    [ textView $ 
        [ height $ WRAP_CONTENT
        , width if os == "IOS" then V (screenWidth unit - 20) else WRAP_CONTENT
        , text (getString EMERGENCY_CONTACTS_SCREEN_DESCRIPTION)
        , color Color.black700
        , padding (Padding 0 10 0 10)
        ] <> FontStyle.paragraphText LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\index item -> contactCardView push state item index) state.data.emergencyContactsData.contactsList)
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
contactCardView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactCardView push state contact index =
  linearLayout
    [ height $ WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 18 18 18 18
    , margin $ Margin 0 5 0 5
    , cornerRadius 8.0
    , stroke ("1," <> Color.grey900)
    ]
    [ linearLayout
        [ height $ V 24
        , width $ V 24
        , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
        , cornerRadius 12.0
        , gravity CENTER
        , margin (MarginRight 10)
        ]
        [ textView $
            [ text (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))
            , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
            ] <> FontStyle.body3 TypoGraphy
        ]
    , textView $
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , weight 1.0
        , text contact.name
        , color Color.black800
        ] <> FontStyle.subHeading1 LanguageStyle
    , textView
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , text (getString REMOVE)
        , color Color.blue900
        , textSize 14
        , onClick push (const (RemoveButtonClicked contact))
        ]
    ]


removeContactPopUpView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]

videoSharedView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
videoSharedView push state = 
    linearLayout [
        orientation VERTICAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
    ][

    ]