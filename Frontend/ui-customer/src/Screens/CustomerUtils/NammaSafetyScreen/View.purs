{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.View
  ( CardViewDataType
  , ContentViewDataType
  , aboutNammaSafetyView
  , aboutSOSData
  , aboutSOSView
  , cardView
  , contactCircleView
  , dashboardView
  , educationView
  , getCardViewData
  , getFirstChar
  , getLastChar
  , getNameInitials
  , measureView
  , nammaSafetyMeasuresView
  , safetyGuidelinesData
  , safetyGuidelinesView
  , safetyMeasuresData
  , screen
  , separatorView
  , settingUpContentView
  , settingUpContentViewData
  , settingUpView
  , sosActiveView
  , stepsHeaderData
  , toggleSwitchView
  , toggleSwitchViewLayout
  , userSettingsView
  , videoRecordSOSView
  , view
  )
  where

import Common.Types.App
import Screens.NammaSafetyScreen.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig)
import Components.GenericHeader as GenericHeader
import Components.StepsHeaderModel as StepsHeaderModel
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.Except.Trans (runExceptT)
import MerchantConfig.DefaultConfig as DC
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (take, (!!), mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (startOtpReciever)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (Unit, bind, const, discard, not, pure, show, unit, when, ($), (&&), (/=), (<<<), (<>), (==), (>=), (||), map)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alignParentBottom, afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageView, id, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, visibility, weight, width, textFromHtml, imageUrl)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.EmergencyContactsScreen.Controller (contactColorsList)
import Screens.Types (NammaSafetyScreenState, Stage(..), StepsHeaderModelState, Contacts)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view
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
      let _ = printLog  "NammasafetyOnboard state -----" state
      eval action state
  }

view
  :: forall w
  . (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state = let
  lang = getValueToLocalStore LANGUAGE_KEY
   in
   linearLayout
   [  height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
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
    , background Color.white900
    , onBackPressed push (const BackPressed )
    ][  
        -- chooseActionSOSView state push
        videoRecordSOSView state push
        -- sosActiveView state push
        -- if state.props.currentStage == NammaSafetyDashboard || state.props.currentStage == AboutNammaSafety then dashboardView state push 
        -- else if state.props.currentStage == SetTriggerCustomerSupport || state.props.currentStage == SetNightTimeSafetyAlert || state.props.currentStage == SetDefaultEmergencyContacts || state.props.currentStage == SetPersonalSafetySettings then settingUpView state push 
        -- else if state.props.currentStage == EduNammaSafetyMeasures || state.props.currentStage == EduNammaSafetyGuidelines || state.props.currentStage == EduNammaSafetyAboutSOS  then educationView state push 
        -- else linearLayout [][]
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

-- ---------------------------------- enterMobileNumberView -----------------------------------
-- enterMobileNumberView:: ST.EnterMobileNumberScreenState  -> String -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
-- enterMobileNumberView  state lang push =
--   linearLayout
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , visibility  if state.props.enterOTP then GONE else VISIBLE
--     , alpha if state.props.enterOTP then 0.0 else 1.0
--     , orientation VERTICAL
--     ][PrestoAnim.animationSet
--       [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 300 10 0 0 true PrestoAnim.Linear
--       ] $ PrimaryEditText.view (push <<< MobileNumberEditTextAction) (mobileNumberEditTextConfig state)
--     , linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       , weight 1.0
--       ][]
--     , PrestoAnim.animationSet
--       ( if EHC.os == "IOS" then [] else [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 400 15 0 0 true PrestoAnim.Linear -- Temporary fix for iOS
--       ]) $ linearLayout
--         [ height WRAP_CONTENT
--         , width MATCH_PARENT
--         , margin (Margin 11 0 0 10)
--         ][ commonTextView state (getString BY_TAPPING_CONTINUE) false Nothing push false
--         , commonTextView state " &nbsp; <u>T&Cs</u>" true (Just (getValueFromConfig "DOCUMENT_LINK")) push true
--           ]
--     , PrestoAnim.animationSet
--       [ Anim.fadeIn $ not state.props.enterOTP
--       , Anim.fadeOut state.props.enterOTP
--       ] $
--       linearLayout
--         [ height WRAP_CONTENT
--         , width MATCH_PARENT
--         ][PrimaryButton.view (push <<< MobileNumberButtonAction) (mobileNumberButtonConfig state)]
--     ]

-- commonTextView :: ST.EnterMobileNumberScreenState -> String -> Boolean -> Maybe String -> (Action -> Effect Unit) -> Boolean -> forall w . PrestoDOM (Effect Unit) w
-- commonTextView state textValue isLink link push isTextFromHtml =
--   textView
--     [ width WRAP_CONTENT
--     , height WRAP_CONTENT
--     , (if isTextFromHtml then textFromHtml else text) textValue
--     , color if isLink then Color.blue900 else Color.black700
--     , textSize FontSize.a_12
--     , fontStyle $ FontStyle.medium LanguageStyle
--     , onClick (\action -> do
--                 when isLink $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" link)--"https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc/view?usp=sharing"
--                 pure unit
--               ) (const TermsAndConditions)
--     ]

-- ------------------------------------- enterOTPView --------------------------------------------
-- enterOTPView:: ST.EnterMobileNumberScreenState -> String -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- enterOTPView state lang push =
--   linearLayout
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , alpha if state.props.enterOTP then 1.0 else 0.0
--     , orientation VERTICAL
--     ][PrestoAnim.animationSet
--       [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig{ifAnim = state.props.enterOTP} --400 15 0 0 state.props.enterOTP PrestoAnim.Linear
--       ] $ PrimaryEditText.view (push <<< OTPEditTextAction) (otpEditTextConfig state)
--     ,  linearLayout
--       [ height WRAP_CONTENT
--       , width WRAP_CONTENT
--       , orientation HORIZONTAL
--       , clickable state.props.resendEnable
--       ][linearLayout
--       [ width WRAP_CONTENT
--       , height WRAP_CONTENT
--       , orientation VERTICAL
--       , alpha if state.props.resendEnable then 1.0 else 0.5
--       ][  textView
--         [ width WRAP_CONTENT
--         , height WRAP_CONTENT
--         , text (getString RESEND)
--         , textSize FontSize.a_12
--         , clickable state.props.resendEnable
--         , fontStyle $ FontStyle.semiBold LanguageStyle
--         , color Color.blue900
--         , onClick push (const Resend)
--         ]
--         , linearLayout
--           [ width MATCH_PARENT
--           , height (V 1)
--           , background Color.blue900
--           ][]
--       ]
--       , textView
--         [ width WRAP_CONTENT
--         , height WRAP_CONTENT
--         , text if lang == "HI_IN" then ("  "<> show state.data.timer <> "s  "<> getString IN) else ("  " <> getString IN <> "  "<> show state.data.timer <> "  s")
--         , textSize FontSize.a_12
--         , lineHeight "22"
--         , fontStyle $ FontStyle.semiBold LanguageStyle
--         , color Color.blue900
--         , visibility if state.props.resendEnable then GONE else VISIBLE
--         ]]
--     , linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       , weight 1.0
--       ][]
--     , PrestoAnim.animationSet
--       [ Anim.fadeIn state.props.enterOTP
--       ] $
--       linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       ][PrimaryButton.view (push <<< VerifyOTPButtonAction) (verifyOTPButtonConfig state)]
--     ]

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
      ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig (getString NAMMA_SAFETY) state)
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , if state.props.currentStage == AboutNammaSafety || state.props.showOnboarding == true then aboutNammaSafetyView state push else userSettingsView state push
      ]
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

cardView:: NammaSafetyScreenState  -> CardViewDataType -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardView state cardData push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 16 20 16 20)
    , margin (Margin 16 16 16 0)
    , stroke ("1,"<>Color.grey900)
    , cornerRadius 8.0
    , onClick push $ const (GoToEducation cardData.stage)
    ][ 
        linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , padding (Padding 16 16 16 16)
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
            , toggleSwitchViewLayout state push
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
                , linearLayout [](mapWithIndex (\index item -> contactCircleView state push item index) state.data.emergencyContacts)
                , PrimaryButton.view (push <<< EditEmergencyContacts) (editEmergencyContactsBtnConfig state)
                
            ]
            , separatorView
            , toggleSwitchViewLayout state push
            , separatorView
            , toggleSwitchViewLayout state push
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.grey700
        , alignParentBottom "true,-1"
        , padding $ Padding 16 20 16 38
        , stroke ("1," <> Color.grey900)
        ][  linearLayout [
            height  WRAP_CONTENT
            , width  MATCH_PARENT
    -- , alpha = if state.props.btnActiveOTP then 1.0 else 0.4
    -- , margin = (MarginLeft 9)
    -- , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
            , gravity CENTER
    -- , padding $ PaddingHorizontal 20 24
            , background Color.grey700
             ][
                textView [
                  textFromHtml ( "<u>" <> ("Know more about Namma Safety")  <> "</u>")
                  , color Color.blue900
                  , textSize $ FontSize.a_14
                  , fontStyle $ FontStyle.semiBold LanguageStyle
                  , onClick push (const ShowAboutNammaSafety)
                ]
             ]  
          ]
    ]

toggleSwitchViewLayout:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchViewLayout state push=
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 16 16 16 16
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  
    textView [ 
        text "Emergency Sharing with contacts"
        , weight 1.0
        , color Color.black800
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle ]
    , toggleSwitchView true push
  ]

contactCircleView ::  NammaSafetyScreenState -> (Action -> Effect Unit) -> Contacts -> Int -> forall w. PrestoDOM (Effect Unit) w
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
  Anim.screenAnimation $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
--   , onBackPressed push $ const BackPressed state.props.showDeleteLocationModel
  ][  PrestoAnim.animationSet
        [ Anim.fadeIn true
        ] $ settingUpContentView (settingUpContentViewData state.props.currentStage) state push
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
                , toggleSwitchView true push
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

        ]
    ]
    , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , alignParentBottom "true,-1"
    ][  PrimaryButton.view (push <<< GoToNextStep) (startNSOnboardingButtonConfig state)
        , if state.props.currentStage /= SetPersonalSafetySettings then PrimaryButton.view (push <<< SkipToNextStep) (skipNSOnboardingButtonConfig state) else linearLayout [][] ]
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
  step :: Int
}



settingUpContentViewData :: Stage -> ContentViewDataType
settingUpContentViewData currentStage = case currentStage of
    SetTriggerCustomerSupport ->  {title: "Trigger alert to NammaYatri support", desc: "We have 24*7 dedicated support who will be alerted automatically", image: "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 0}
    SetNightTimeSafetyAlert ->  {title: "Enable night time safety alerts?", desc: "To ensure your safety, from 9 PM-6AM, \nwe would send safety check alerts basis anomaly detection. In cases of route-deviation or when vehicle is not moving", image: "ny_ic_emergency_contacts,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 1}
    SetDefaultEmergencyContacts ->  {title: "Share Info with emergency contacts?", desc: "On SOS, the ride information will be auto shared with the below emergency contacts.", image: "", step : 2}
    SetPersonalSafetySettings ->  {title: "Almost Done!", desc: "During SOS, based on criticality of situation, any of these options can be selected\n\n \t• Call 112\n \t• Call NammaYatri support\n \t• Record video", image: "", step : 3}
    _ -> {title:"", desc:"", image:"", step : 0}


------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height (V 1)
  , width MATCH_PARENT
  , margin (Margin 0 16 0 16)
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
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
        GenericHeader.view (push <<< GenericHeaderACEdu) (genericHeaderConfig "Namma Safety Measures" state)
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
        , if state.props.currentStage == EduNammaSafetyMeasures then nammaSafetyMeasuresView state
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
                    measureView item true
                ) safetyMeasuresData) 
    ]

measureView :: String -> Boolean -> forall w . PrestoDOM (Effect Unit) w
measureView text' isCorrect = 
    linearLayout [
                margin $ MarginBottom 16
                , gravity CENTER
            ][
                imageView [
                    imageWithFallback if isCorrect then "ny_ic_tick,user/nammaYatri/res/drawable/ny_ic_tick.png" else "ny_ic_cross,user/nammaYatri/res/drawable/ny_ic_cross.png"
                    , height $ V 20
                    , width $ V 20
                    , margin $ MarginRight 16
                ]
                , textView [
                    text text'
                ]
            ]

safetyMeasuresData :: Array String
safetyMeasuresData = [
    "Night-time safety check based on ride anomaly detection",
    "24/7 dedicated NammaSafety support",
    "SOS Namma Safety button.",
    "Zero tolerance policy for drivers/customers",
    "Customer privacy - drivers can't view the exact address of the ride once completed"
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
                    measureView item true
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
                    measureView item true
                ) aboutSOSData) 
    ]

aboutSOSData :: Array String
aboutSOSData = [
    "Driver re-routing to different remote locations",
    "Inappropriate verbal/physical cues from the drive",
    "Other male passengers asked to join ride without your consent",
    "Driver stopped unsolicited, at a remote area",
    "Fare issues",
    "Drunk driving",
    "Rash driving",
    "Vehicle number mismatch"
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
      ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig "Emergency SOS" state)
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 33 70 33 30
                , gravity CENTER
        , orientation VERTICAL
        ][
            linearLayout [
                height $ V 188
                , width $ V 188
                , background $ Color.red
                -- , gravity CENTER
            ][]
            , linearLayout[
                margin $ MarginTop 24
                -- , layoutGravity CENTER
                , orientation VERTICAL
            ][
                textView [
                    text "Emergency SOS is getting activated..."
                    , textSize FontSize.a_16
                    , margin $ MarginBottom 20
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color $ Color.white900
                ]
                , textView [
                    text "Share info with emergency contacts"
                    , textSize FontSize.a_14
                    , margin $ MarginBottom 13
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color $ Color.white900
                ]
                , textView [
                    text "Trigger alert to Nammayatri support"
                    , textSize FontSize.a_14
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color $ Color.white900
                ]
            ]
        ]
        , PrimaryButton.view (push <<< GoToNextStep) (cancelSOSBtnConfig state)
      ]
  ]

chooseActionSOSView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
chooseActionSOSView state push = 
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
      ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig "Emergency SOS" state)
    , linearLayout
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
            , textView [
                text "Please stay calm, Your real time location is being shared with your emergency contacts and our support team"
                    , textSize FontSize.a_14
                    , margin $ MarginBottom 12
                    , fontStyle $ FontStyle.semiBold LanguageStyle
                    , color $ Color.white900
            ]
            , linearLayout [
                width MATCH_PARENT
               , height WRAP_CONTENT
               , orientation HORIZONTAL
            --    , padding $ Padding 0 16 16 16
               , margin $ MarginTop 8
              ]
              [
                imageView [
                    imageWithFallback "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
                    , height $ V 30
                    , width $ V 30
                    , margin $ MarginRight 8
                ]
                , textView [ text "Sharing With:"
                    , height MATCH_PARENT
                    , color Color.white900
                    , textSize FontSize.a_12
                    , margin $ MarginRight 8
                    , gravity CENTER
                    , fontStyle $ FontStyle.medium LanguageStyle 
                ] 
                , linearLayout [](mapWithIndex (\index item -> contactCircleView state push item index) state.data.emergencyContacts)                
              ]
            ,linearLayout [
                height $ V 300
                , width MATCH_PARENT
                , background $ Color.red
                , margin $ MarginTop 37
                -- , gravity CENTER
            ][]
            , linearLayout[
                width MATCH_PARENT
                , margin $ MarginTop 24
                , gravity CENTER
                , orientation HORIZONTAL
            ][
                linearLayout[
                orientation VERTICAL
                , gravity CENTER
                , margin $ MarginHorizontal 5 5
                ][
                    imageView [
                        imageWithFallback "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 55
                        , width $ V 55
                        , background $ Color.white900
                        , cornerRadius 87.0
                        , padding $ Padding 15 15 15 15
                    ]
                    , textView [
                        text "Call Police (112)"
                        , color Color.white900
                        , textSize FontSize.a_12
                        , fontStyle $ FontStyle.semiBold LanguageStyle 
                    ]
                ]
                , linearLayout[
                    orientation VERTICAL
                    , gravity CENTER
                    , margin $ MarginHorizontal 5 5
                ][
                    imageView [
                        imageWithFallback "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 55
                        , width $ V 55
                        , background $ Color.white900
                        , cornerRadius 87.0
                        , padding $ Padding 15 15 15 15
                    ]
                    , textView [
                        text "Call Police (112)"
                        , color Color.white900
                        , textSize FontSize.a_12
                        , fontStyle $ FontStyle.semiBold LanguageStyle 
                    ]
                ]
                , linearLayout[
                    orientation VERTICAL
                    , gravity CENTER
                    , margin $ MarginHorizontal 5 5
                ][
                    imageView [
                        imageWithFallback "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 55
                        , width $ V 55
                        , background $ Color.white900
                        , cornerRadius 87.0
                        , padding $ Padding 15 15 15 15
                    ]
                    , textView [
                        text "Call Police (112)"
                        , color Color.white900
                        , textSize FontSize.a_12
                        , fontStyle $ FontStyle.semiBold LanguageStyle 
                    ]
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
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig "Emergency Video" state)
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ][
            linearLayout [
                width MATCH_PARENT
               , height WRAP_CONTENT
               , orientation HORIZONTAL
            --    , padding $ Padding 0 16 16 16
               , margin $ MarginTop 8
              ]
              [
                imageView [
                    imageWithFallback "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
                    , height $ V 30
                    , width $ V 30
                    , margin $ MarginRight 8
                ]
                , textView [ text "Sharing With:"
                    , height MATCH_PARENT
                    , color Color.white900
                    , textSize FontSize.a_12
                    , margin $ MarginRight 8
                    , gravity CENTER
                    , fontStyle $ FontStyle.medium LanguageStyle 
                ] 
                , linearLayout [](mapWithIndex (\index item -> contactCircleView state push item index) state.data.emergencyContacts)                
              ]
            ,linearLayout [
                height $ V 300
                , width MATCH_PARENT
                , background $ Color.red
                , margin $ MarginTop 37
                , id $ EHC.getNewIDWithTag "VideoCamView"
                -- , gravity CENTER
            ][]
            , linearLayout[
                width MATCH_PARENT
                , margin $ MarginTop 24
                , gravity CENTER
                , orientation HORIZONTAL
            ][
                linearLayout[
                orientation VERTICAL
                , gravity CENTER
                , margin $ MarginHorizontal 5 5
                ][
                    imageView [
                        imageWithFallback "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                        , height $ V 55
                        , width $ V 55
                        , background $ Color.white900
                        , cornerRadius 87.0
                        , padding $ Padding 15 15 15 15
                    ]
                    , textView [
                        text "Call Police (112)"
                        , color Color.white900
                        , textSize FontSize.a_12
                        , fontStyle $ FontStyle.semiBold LanguageStyle 
                    ]
                ]
                
            ]
            , PrimaryButton.view (push <<< GoToNextStep) (cancelSOSBtnConfig state)
        ]
      ]
  ]

toggleSwitchView :: Boolean -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchView isActive push = 
    linearLayout[
        height MATCH_PARENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
    ][
        imageView [
            imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
            , width $ V 40
            , height $ V 20
        ]  
    ]