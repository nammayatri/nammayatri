{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ActivateSafetyScreen.View where

import Animation
import Prelude
import PrestoDOM
import Screens.NammaSafetyFlow.ComponentConfig
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
import Mobility.Prelude
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyFlow.Components.ContactsList
import Screens.EmergencyContactsScreen.View (getFirstChar, getLastChar)
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.HelperViews
import Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.Types (NewContacts, RecordingState(..), SafetySetupStage(..), StepsHeaderModelState, NammaSafetyScreenState)
import Screens.Types as ST
import Services.API (GetSosDetailsRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Timers

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "ActivateSafetyScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  response <- Remote.getEmergencySettingsBT ""
                  lift $ lift $ doAff do liftEffect $ push $ UpdateEmergencySettings response
                  if initialState.data.sosId == "" then do
                    (GetSosDetailsRes sosDetails) <- Remote.getSosDetails initialState.data.rideId
                    case sosDetails.sosId of
                      Just id -> do
                        lift $ lift $ doAff do liftEffect $ push $ UpdateSosId id
                        pure unit
                      Nothing -> pure unit
                  else
                    lift $ lift $ doAff do liftEffect $ push $ GoToActiveSos
                  lift $ lift $ doAff do liftEffect $ push $ DisableShimmer
                  pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "ActivateSafetyScreen action " action
        let
          _ = spy "ActivateSafetyScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const $ BackPressed
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.black900
            , padding padding'
            ]
            [ case state.props.showTestDrill of
                true -> testSafetyHeaderView push
                false -> Header.view (push <<< SafetyHeaderAction) headerConfig
            , case state.props.confirmTestDrill, state.props.triggeringSos, state.props.showCallPolice of
                true, _, _ -> confirmSafetyDrillView state push
                _, _, true -> dialPoliceView state push
                false, true, _ -> triggeringSosView state push
                false, false, _ -> activateSafetyView state push
            ]
        , shimmerView state
        ]
  where
  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig =
    Header.config
      { learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY
      , showLearnMore = true
      , useLightColor = true
      , title = getString SAFETY_CENTER
      , headerVisiblity =
        case state.props.confirmTestDrill of
          true -> INVISIBLE
          false -> VISIBLE
      }

------------------------------------- dashboardView -----------------------------------
activateSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
activateSafetyView state push =
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ sosButtonView state push
        , emergencyContactsView state push
        , otherActionsView state push
        ]
    ]

-- ---------------------------------- sosButtonView -----------------------------------
sosButtonView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosButtonView state push =
  let
    buttonText = case state.props.triggeringSos, state.props.showTestDrill of
      true, _ -> show state.props.timerValue
      false, true -> "TEST\nSOS"
      false, false -> "SOS" --getString SOS

    descText = case state.props.triggeringSos, state.props.showTestDrill of
      true, true -> "Test SOS is getting activated in..."
      true, false -> "Emergency SOS is getting activated in..."
      false, true -> "Press the below button to start the\nTest SOS Drill"
      false, false -> "Press the button on emergency"
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ]
      [ textView
          $ [ text descText
            , color Color.white900
            , margin $ MarginTop 20
            ]
          <> FontStyle.h3 TypoGraphy
      , relativeLayout
          ( [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginVertical 40 40
            , gravity CENTER
            ]
              <> if not state.props.triggeringSos then
                  [ onClick
                      ( \action -> do
                          void $ startTimer state.props.timerValue "triggerSos" "1" push CountDown
                          void $ push action
                      )
                      (const TriggerSosCountdown)
                  ]
                else
                  []
          )
          [ imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_sos_button"
              , width $ V 188
              , height $ V 188
              ]
          , textView
              $ [ text buttonText
                , color Color.white900
                , gravity CENTER
                , width $ V 188
                , height $ V 188
                ]
              <> (if state.props.triggeringSos then FontStyle.title0 else FontStyle.priceFont) TypoGraphy
          ]
      ]

type ImageTextViewConfig
  = { text' :: String
    , isActive :: Boolean
    , textColor :: String
    , useMargin :: Boolean
    , usePadding :: Boolean
    , useFullWidth :: Boolean
    , image :: Maybe String
    }

imageWithTextView :: ImageTextViewConfig -> forall w. PrestoDOM (Effect Unit) w
imageWithTextView config =
  linearLayout
    ( [ height WRAP_CONTENT
      , width if config.useFullWidth then MATCH_PARENT else WRAP_CONTENT
      ]
        <> if config.useMargin then
            [ margin $ MarginTop 12 ]
          else
            []
              <> if config.usePadding then [ padding $ PaddingHorizontal 16 16 ] else []
    )
    [ imageView
        [ imageWithFallback
            $ fetchImage FF_ASSET case config.image of
                Just image' -> image'
                Nothing -> if config.isActive then "ny_ic_check" else "ny_ic_ellipse_outline_grey"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        ]
    , textView
        $ [ text config.text'
          , color config.textColor
          , weight 1.0
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> FontStyle.tags TypoGraphy
    ]

emergencyContactsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
emergencyContactsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , background Color.blackOpacity12
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ not state.props.triggeringSos
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ not state.props.showTestDrill
        ]
        [ imageWithTextView configDescOne
        , imageWithTextView configDescTwo
        ]
    , textView
        $ [ text "Emergency Contacts can follow/ take emergency response actions on Namma Yatri App"
          , color Color.white900
          , visibility $ boolToVisibility state.props.showTestDrill
          , gravity CENTER
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 12 12 12 12
        , orientation VERTICAL
        , background Color.black900
        , margin $ MarginTop 16
        , cornerRadius 12.0
        ]
        [ textView
            $ [ text "Select the contact to be called"
              , color Color.black500
              ]
            <> FontStyle.tags TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 12
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , gravity LEFT
                ]
                ( mapWithIndex
                    ( \index item ->
                        ContactCircle.view (ContactCircle.getContactConfig item index true) (push <<< ContactAction)
                    )
                    state.data.contactsList
                )
            , layoutWithWeight
            , textView
                $ [ textFromHtml $ "<u>" <> getString EDIT_ACTIONS <> "</u>"
                  , color Color.blue800
                  , padding $ Padding 16 6 16 6
                  , onClick push $ const AddContacts
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    , textView
        $ [ text "Please inform your emergency contacts about this test drill to prevent any unnecessary panic"
          , color Color.black500
          , visibility $ boolToVisibility state.props.showTestDrill
          , margin $ MarginTop 16
          ]
        <> FontStyle.tags TypoGraphy
    ]
  where
  configDescOne =
    { text': getString SAFETY_TEAM_WILL_BE_ALERTED
    , isActive: true
    , textColor: Color.white900
    , useMargin: false
    , useFullWidth: true
    , usePadding: false
    , image: Nothing
    }

  configDescTwo =
    { text': getString EMERGENCY_CONTACTS_CAN_TAKE_ACTION
    , isActive: true
    , textColor: Color.white900
    , useMargin: true
    , useFullWidth: true
    , usePadding: false
    , image: Nothing
    }

otherActionsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
otherActionsView state push =
  frameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , visibility $ boolToVisibility $ not state.props.triggeringSos
    ]
    [ linearLayout
        ( [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ Margin 16 16 16 16
          , cornerRadius 12.0
          ]
            <> if state.props.showTestDrill then
                [ alpha 0.5 ]
              else
                []
        )
        [ textView
            $ [ text "Other Safety Actions"
              , color Color.white900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , textView
            $ [ text "Available only during a real SOS situation"
              , color Color.white900
              , margin $ MarginTop 4
              ]
            <> FontStyle.captions TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 20
            , onClick push $ const ShowPoliceView
            ]
            [ imageWithTextView configActionOne
            , layoutWithWeight
            , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
                , height $ V 20
                , width $ V 20
                ]
            ]
        , separatorView Color.black700 $ MarginVertical 16 16
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , onClick push $ const ShowSafetyIssueView
            ]
            [ imageWithTextView configActionTwo
            , layoutWithWeight
            , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
                , height $ V 20
                , width $ V 20
                ]
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , clickable state.props.showTestDrill
        ]
        []
    ]
  where
  configActionOne =
    { text': getString CALL_POLICE
    , isActive: true
    , textColor: Color.white900
    , useMargin: false
    , usePadding: false
    , useFullWidth: false
    , image: Just "ny_ic_police"
    }

  configActionTwo =
    { text': getString REPORT_SAFETY_ISSUE
    , isActive: true
    , textColor: Color.white900
    , useMargin: false
    , usePadding: false
    , useFullWidth: false
    , image: Just "ny_ic_issue_box"
    }

getHeaderTitle :: SafetySetupStage -> String
getHeaderTitle stage = getString NAMMA_SAFETY

toggleSwitchView :: Boolean -> Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive visibility' action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action --ToggleSwitch stage
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]

shimmerView :: forall w. ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
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

testSafetyHeaderView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
testSafetyHeaderView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.yellow800
    , padding $ Padding 16 16 16 16
    , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , orientation VERTICAL
        ]
        [ textView
            $ [ text "Test Safety Drill"
              , color Color.black900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , textView
            [ text "This is not a real SOS situation"
            , color Color.black900
            ]
        ]
    , layoutWithWeight
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        [ textView
            [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
            , color Color.black900
            , gravity RIGHT
            , margin $ MarginRight 16
            -- , onClick push $ const $ SwitchToStage AboutNammaSafety
            ]
        ]
    ]

disclaimerView :: forall w. NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
disclaimerView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ state.props.triggeringSos && not state.props.showTestDrill
    ]
    [ textView
        $ [ text "Disclaimer:"
          , color Color.white900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 16
        , orientation VERTICAL
        ]
        ( mapWithIndex
            ( \index item ->
                measureView
                  { text': item.text
                  , showBullet: true
                  , isCorrect: false
                  , color': Color.white900
                  , marginBottom: 10
                  , style: FontStyle.Body1
                  , action: item.action
                  }
                  push
            )
            disclaimerText
        )
    ]
  where
  disclaimerText =
    [ { text: "Use only in emergencies", action: NoAction }
    , { text: "Use <span style='color:#2194FF'><u>Test safety drill</u></span> for testing", action: GoToTestDrill }
    , { text: "Misuse may result in legal action", action: NoAction }
    ]

dismissSoSButtonView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dismissSoSButtonView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 16
    , orientation VERTICAL
    , visibility $ boolToVisibility state.props.triggeringSos
    ]
    [ case state.props.showTestDrill of
        true -> emptyTextView
        false -> separatorView Color.black700 $ MarginVertical 40 16
    , textView
        $ [ text "NammaYatri will indicate to your emergency contacts that this is a test drill, ensuring a stress-free experience."
          , color Color.black500
          , gravity CENTER
          , margin $ Margin 16 16 16 0
          , visibility $ boolToVisibility state.props.showTestDrill
          ]
        <> FontStyle.tags TypoGraphy
    , PrimaryButton.view (push <<< CancelSosTrigger) $ dismissSoSButtonConfig state
    ]

type MeasureViewConfig
  = { text' :: String
    , showBullet :: Boolean
    , isCorrect :: Boolean
    , color' :: String
    , marginBottom :: Int
    , style :: FontStyle.Style
    , action :: Action
    }

measureView :: forall w. MeasureViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
measureView { text', showBullet, isCorrect, color', marginBottom, style, action } push =
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
        $ [ textFromHtml text'
          , color color'
          , weight 1.0
          , gravity LEFT
          , onClick push $ const action
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    ]

------------------- separator -------------------
separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

triggeringSosView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
triggeringSosView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ sosButtonView state push
    , layoutWithWeight
    , disclaimerView state push
    , dismissSoSButtonView state push
    ]

confirmSafetyDrillView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
confirmSafetyDrillView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_start_test_drill"
        , width MATCH_PARENT
        , height $ V 300
        , margin $ MarginTop 120
        ]
    , layoutWithWeight
    , textView
        $ [ text "Are you ready to start the test safety drill?"
          , color Color.white900
          , gravity CENTER
          ]
        <> FontStyle.h1 TypoGraphy
    , textView
        $ [ text "Taking part in the test drill helps you to understand how to act in a real emergency. Take the test drill now, to avoid panic during an emergency."
          , color Color.white900
          , gravity CENTER
          , margin $ MarginTop 16
          ]
        <> FontStyle.body5 TypoGraphy
    , PrimaryButton.view (push <<< StartTestDrill) $ startTestDrillButtonConfig state
    ]

dialPoliceView :: forall w. NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dialPoliceView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.black700
        , background Color.blackOpacity12
        , orientation VERTICAL
        , margin $ MarginVertical 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 12.0
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView 
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_map_pin_white" 
              , height $ V 24 
              , width $ V 24 
              , margin $ MarginRight 10
              ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text "Your Current Location"
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text "Police will be alerted and your emergency contacts will be notified"
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 16
            ]
            [ imageView 
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_vehicle_details"
              , height $ V 24 
              , width $ V 24 
              , margin $ MarginRight 10
              ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text "Your Vehicle Info"
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text "Police will be alerted and your emergency contacts will be notified"
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
          , separatorView Color.black500 $ Margin 16 16 16 16
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ][
              imageView 
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
              , height $ V 24 
              , width $ V 24 
              , margin $ MarginRight 5
              ]
            , textView $
              [ text "Please give the operator your location - The app doesn’t share the location automatically."
              , color Color.black500
              ] <> FontStyle.tags TypoGraphy
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , margin $ MarginBottom 16
        ]
        [ callPoliceView state push
        ]
    ]

callPoliceView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      ]
        <> if state.props.showTestDrill then [ alpha 0.6 ] else [ onClick push $ const $ CallPolice ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
        , height $ V 26
        , width $ V 26
        ]
    , textView
        $ [ text $ getString CALL_POLICE
          , gravity CENTER
          , color Color.white900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]
