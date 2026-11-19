{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SetupSafetySettingsScreen.View where

import Animation (fadeIn, screenAnimation)
import Prelude (Unit, bind, const, discard, map, not, pure, unit, void, ($), (&&), (+), (/=), (<<<), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, alignParentBottom, background, color, cornerRadius, gravity, height, id, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, shimmerFrameLayout, stroke, text, textFromHtml, textView, visibility, weight, width, accessibilityHint)
import Screens.NammaSafetyFlow.ComponentConfig (continueNextStepButtonConfig, removeContactPopUpModelConfig)
import Screens.NammaSafetyFlow.Components.HelperViews (emptyTextView, recommendContactsToInstallView, shimmerView)
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, null, (..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, getDefaultPixelSize)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyFlow.Components.ContactsList as ContactsList
import Screens.NammaSafetyFlow.SetupSafetySettingsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Data.Either (Either(..))

screen :: ST.NammaSafetyScreenState -> Screen Action ST.NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SetupSafetySettingsScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState
              $ do
                  eiResponse <- Remote.getEmergencySettings ""
                  case eiResponse of
                    Right response -> do
                      EHC.liftFlow $ push $ UpdateEmergencySettings response
                      EHC.liftFlow $ push $ DisableShimmer
                      EHU.toggleLoader false
                    Left err -> do
                      let errMessage = if err.code == 400 
                                          then err.response.errorMessage 
                                          else getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                      void $ pure $ EHU.showToast errMessage
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SetupSafetySettingsScreen action " action
        let
          _ = spy "SetupSafetySettingsScreen state " state
        eval action state
  }

view ::
  forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.white900
            , padding padding'
            ]
            [ settingUpView state push
            , shimmerView state
            ]
        , if state.props.showInfoPopUp then removeContactPopUpView push state else emptyTextView
        ]
  where
  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

-- ---------------------------------- settingUpView -----------------------------------
settingUpView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
settingUpView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    ]
    [ PrestoAnim.animationSet
        [ fadeIn true
        ]
        $ settingUpContentView (settingUpContentViewData state) state push
    ]

settingUpContentView :: ContentViewDataType -> ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
settingUpContentView config state push =
  let
    footerBounds = JB.getLayoutBounds $ EHC.getNewIDWithTag "SettingUpContentViewFooter"
    exactHeightFooter = getDefaultPixelSize footerBounds.height
  in
    relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding $ PaddingBottom $ exactHeightFooter + 24
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , id $ EHC.getNewIDWithTag "SettingUpContentViewHeader"
              , orientation VERTICAL
              ]
              [ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderData config.step)
              ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , weight 1.0
            ][
            scrollView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              ]
              [ linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , padding $ Padding 16 32 16 0
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
                              , margin $ MarginRight 14
                              , width $ V 50
                              , visibility $ boolToVisibility $ config.image /= ""
                              ]
                          ]
                      , toggleSwitchView config.isActive state.props.setupStage push
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
                        , textFromHtml config.desc
                        , color Color.black700
                        , margin $ MarginTop 8
                        ]
                      <> FontStyle.body5 TypoGraphy
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , visibility $ boolToVisibility $ state.props.setupStage == ST.SetDefaultEmergencyContacts
                      , orientation VERTICAL
                      ]
                      [ ContactsList.view (push <<< ContactListAction) state.data.emergencyContactsList
                      ]
                  , linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , visibility $ boolToVisibility $ state.props.setupStage == ST.SetDefaultEmergencyContacts && (not $ null state.data.emergencyContactsList)
                      ]
                      [ recommendContactsToInstallView state.props.appName true
                      ]
                  ]
                ]
              ]
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , id $ EHC.getNewIDWithTag "SettingUpContentViewFooter"
          , alignParentBottom "true,-1"
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              , alignParentBottom "true,-1"
              ]
              [ PrimaryButton.view (push <<< GoToNextStep) (continueNextStepButtonConfig state) ]
          ]
      ]

stepsHeaderData :: Int -> ST.StepsHeaderModelState
stepsHeaderData currentIndex =
  { activeIndex: currentIndex
  , textArray: [ getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS ]
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

settingUpContentViewData :: ST.NammaSafetyScreenState -> ContentViewDataType
settingUpContentViewData state = case state.props.setupStage of
  ST.SetDefaultEmergencyContacts ->
    { title: getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_TITLE
    , desc:
        getString
          if null state.data.emergencyContactsList then
            CHOOSE_RESPONSIVE_CONTACTS
          else
            SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC
    , image: "ny_ic_share"
    , step: 0
    , isActive: state.data.shareToEmergencyContacts && length state.data.emergencyContactsList /= 0
    }
  ST.SetNightTimeSafetyAlert ->
    { title: getString ENABLE_NIGHT_TIME_SAFETY_ALERTS_TITLE
    , desc: getString ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC
    , image: "ny_ic_night_safety"
    , step: 1
    , isActive: state.data.nightSafetyChecks
    }
  ST.SetPersonalSafetySettings ->
    { title: getString ALMOST_DONE_TITLE
    , desc: getString ALMOST_DONE_DESC
    , image: ""
    , step: 2
    , isActive: state.data.nightSafetyChecks
    }

removeContactPopUpView :: forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]

getSafePadding :: Padding
getSafePadding = Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)

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
            ( \_ ->
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

toggleSwitchView :: Boolean -> ST.SafetySetupStage -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive stage push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const $ ToggleSwitch stage
    , visibility $ boolToVisibility $ stage /= ST.SetPersonalSafetySettings
    , accessibilityHint $ (if isActive then "enable " else "disable ") <> accessibilityHintText
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]
    where 
    accessibilityHintText = case stage of 
      ST.SetDefaultEmergencyContacts -> "Share Emergency Contacts Button"
      ST.SetNightTimeSafetyAlert -> "Night time safety check Button"
      _ -> ""

measureView :: String -> Boolean -> Boolean -> String -> Int -> FontStyle.Style -> forall w. PrestoDOM (Effect Unit) w
measureView text' showBullet isCorrect color' marginBottom style =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
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
    , if not showBullet then
        imageView
          [ imageWithFallback $ fetchImage FF_ASSET if isCorrect then "ny_ic_tick_green" else "ny_ic_cross"
          , height $ V 20
          , width $ V 20
          , margin $ MarginRight 16
          , visibility $ boolToVisibility $ not showBullet
          ]
      else
        emptyTextView
    , textView
        $ [ text text'
          , color color'
          , weight 1.0
          , gravity LEFT
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    ]
