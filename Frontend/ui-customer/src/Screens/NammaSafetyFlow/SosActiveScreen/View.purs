{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SosActiveScreen.View where

import PrestoDOM.Animation as PrestoAnim
import Animation (screenAnimationFadeInOut, fadeIn, fadeOut, screenAnimation)
import Prelude (Unit, const, discard, not, pure, unit, void, ($), (&&), (<<<), (<>), (==), (<#>), map, (/), (-), (/=), show, when, bind)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alignParentBottom, alpha, background, color, cornerRadius, gravity, height, id, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, relativeLayout, rippleColor, scrollView, stroke, text, textView, visibility, weight, width, accessibilityHint, maxLines, ellipsize, fillViewport, enableAnimateOnGone)
import Screens.NammaSafetyFlow.ComponentConfig (cancelSOSBtnConfig, safetyAudioRecordingConfig)
import Screens.NammaSafetyFlow.Components.HelperViews (layoutWithWeight, safetyPartnerView, separatorView, emptyTextView)
import Common.Types.App as CTA
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, null)
import Data.Maybe (Maybe)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, getLocationName, getAssetsBaseUrl)
import JBridge as JB
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (doAff)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.SosActiveScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Screens.NammaSafetyFlow.Components.SafetyUtils (getVehicleDetails, getPrimaryContact)
import Components.Safety.SosButtonAndDescription as SosButtonAndDescription
import Data.Maybe as Mb
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import Components.Safety.Utils as SU
import Components.RecordAudioModel.View as RecordAudioModel
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple as DT
import Effect.Uncurried (runEffectFn2)
import Components.Safety.SafetyAudioRecording as SafetyAudioRecording
import Data.Function.Uncurried (runFn2)

screen :: ST.NammaSafetyScreenState -> Screen Action ST.NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SosActiveScreen"
  , globalEvents:
      [ ( \push -> do
            void $ pure $ runFn2 JB.storeOnPauseCallback push OnPauseCallback
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  when initialState.data.autoCallDefaultContact $ lift $ lift $ doAff do liftEffect $ push $ PlaceCall
                  pure unit
            void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SosActiveScreen action " action
        let
          _ = spy "SosActiveScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation $ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black900
    , padding padding'
    , onBackPressed push $ const $ BackPressed
    , afterRender
        ( \_ -> do
            getLocationName push 9.9 9.9 "Current Location" SelectedCurrentLocation
            pure unit
        )
        (const NoAction)
    ]
    [ Header.view (push <<< SafetyHeaderAction) headerConfig
    , case state.props.showTestDrill of
        true -> Header.testSafetyHeaderView (push <<< SafetyHeaderAction)
        false -> emptyTextView
    , case state.props.showCallPolice of
        true -> dialPoliceView state push
        false -> sosActiveView state push
    ]
  where
  padding' = if EHC.os == "IOS" then (PaddingVertical EHC.safeMarginTop (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig =
    (Header.config CTA.Language)
      { useLightColor = true
      , title = getStringWithoutNewLine $ if not state.props.showCallPolice then EMERGENCY_SOS else CALL_POLICE
      , learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY
      , showLearnMore = false
      }

sosActiveView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActiveView state push =
  linearLayout
  [ weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  ]
  [ linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    ]
    [ scrollView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , fillViewport true
      ][ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          [ SosButtonAndDescription.view (push <<< SosButtonAndDescriptionAction) $ emergencySosConfig state   
          , emergencyContactsView state push
          , SafetyAudioRecording.view (push <<< SafetyAudioRecordingAction) $ safetyAudioRecordingConfig state
          , sosActionTilesView state push
          ]
      ]
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , orientation VERTICAL
    ]
    [ separatorView Color.black800 $ MarginTop 0
    , PrimaryButton.view (push <<< MarkRideAsSafe) (cancelSOSBtnConfig state)
    ]
  ]
  
  

emergencySosConfig :: ST.NammaSafetyScreenState -> SosButtonAndDescription.Config
emergencySosConfig state =
  SosButtonAndDescription.config
  { 
    sosDescription = [ measureViewConfig {text' = getString SAFETY_TEAM_CALLBACK_REQUESTED} ]
                     <> if null state.data.emergencyContactsList then [] else [measureViewConfig {text' = getString EMERGENCY_CONTACTS_NOTIFIED}]
                     <> case contactName, state.data.autoCallDefaultContact of
                          Mb.Just name, true -> [ measureViewConfig {text' = getString CALL_PLACED <> ": " <> name} ]
                          _,_ -> []
  , descriptionText = getString if state.props.showTestDrill then TEST_SOS_ACTIVATED else EMERGENCY_SOS_ACTIVATED
  , showSosButton = false
  }
  where
    measureViewConfig =
      { text': ""
      , isActive: true
      , textColor: Color.white900
      , useMargin: true
      , useFullWidth: true
      , usePadding: false
      , image: Mb.Nothing
      , visibility: true
      , textStyle: FontStyle.Tags
      , showBullet: false
      }

    contactName = (getPrimaryContact state) <#> _.name

sosActionTilesView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActionTilesView state push =
  PrestoAnim.animationSet 
    [ fadeOut state.props.isAudioRecordingActive , fadeIn (not state.props.isAudioRecordingActive) ]
    $
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , visibility $ boolToVisibility $ not state.props.isAudioRecordingActive 
    , enableAnimateOnGone true
    ]
    [ safetyActionRow actionsRow1 push
    , safetyActionRow actionsRow2 push
    ]

  where
    actionsRow1 =
        [ { text: getStringWithoutNewLine RECORD_AUDIO, image: fetchImage COMMON_ASSET "ny_ic_microphone_white", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< RecordAudio, isDisabled: false }
        , { text: getStringWithoutNewLine CALL_POLICE , image: fetchImage COMMON_ASSET "ny_ic_police_alert" , backgroundColor: Color.redOpacity20, strokeColor: Color.redOpacity30, push : push <<< ShowPoliceView, isDisabled: state.props.showTestDrill }
        ]

    actionsRow2 =
        [ { text: getString SIREN, image: sirenImage, backgroundColor: sirenTileBackgroundColor, strokeColor: sirenTileStrokeColor, push : push <<< ToggleSiren, isDisabled: false }
        , { text: getStringWithoutNewLine CALL_SAFETY_TEAM, image: fetchImage FF_ASSET "ny_ic_support_unfilled", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< CallSafetyTeam, isDisabled: state.props.showTestDrill }
        ]

    safetyActionRow actionItems push =
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , margin $ Margin 10 12 10 0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER
            ]
            ( map
                ( \item ->
                    SafetyActionTileView.view item.image item.text item.push item.backgroundColor item.strokeColor (V $ (EHC.screenWidth unit - 44) / 2) true item.isDisabled Color.white900 WRAP_CONTENT
                )
                actionItems
            )
        ]
      
    sirenImage = fetchImage COMMON_ASSET if state.props.triggerSiren then "ny_ic_full_volume" else "ny_ic_no_volume"
    sirenTileBackgroundColor = if state.props.triggerSiren then Color.black712 else Color.blackOpacity12
    sirenTileStrokeColor = if state.props.triggerSiren then Color.black700 else Color.black800

sosDescriptionView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosDescriptionView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , weight 1.0
    ]
    [ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ Margin 16 16 16 0
            , gravity CENTER
            ]
            [ textView
                $ [ text title
                  , color Color.white900
                  ]
                <> FontStyle.h3 CTA.TypoGraphy
            , textView
                $ [ text desc
                  , color Color.white900
                  , margin $ MarginTop 8
                  , gravity CENTER
                  ]
                <> FontStyle.body1 CTA.TypoGraphy
            , linearLayout
                [ width $ V if not $ null state.data.emergencyContactsList then 200 else 250
                , height $ V if not $ null state.data.emergencyContactsList then 180 else 250
                , margin $ MarginTop 20
                , gravity CENTER
                ]
                [ screenAnimationFadeInOut
                    $ lottieAnimationView
                        [ id $ EHC.getNewIDWithTag "SafetylottieAnimationView"
                        , onAnimationEnd
                            ( \_ -> do
                                void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig { rawJson = (getAssetsBaseUrl CTA.FunctionCall) <> "lottie/ny_ic_sos_active.json", lottieId = EHC.getNewIDWithTag "SafetylottieAnimationView", scaleType = "FIT_CENTER", repeat = true }
                            )
                            (const NoAction)
                        , height MATCH_PARENT
                        , width MATCH_PARENT
                        ]
                ]
            ]
        ]
    ]
  where
  title = getString if state.props.showTestDrill then TEST_EMERGENCY_REQUEST_SENT else EMERGENCY_REQUEST_SENT

  desc =
    getString
      $ if state.props.showTestDrill then
          TEST_SOS_TRIGGERED_DESC
        else if null state.data.emergencyContactsList then
          PLEASE_STAY_CALM_TEAM_ALERTED state.props.appName
        else
          SOS_TRIGGERED_DESC

sosActionsView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActionsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        , gravity CENTER
        ]
        [ textView
            $ [ text $ getString TRY_ANOTHER_CONTACT
              , color Color.white900
              , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
              ]
            <> FontStyle.paragraphText CTA.TypoGraphy
        , emergencyContactsView state push
        ]
    ]

emergencyContactsView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
emergencyContactsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , background Color.blackOpacity12
    , orientation VERTICAL
    , margin $ Margin 16 8 16 0
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
    ]
    [ textView $ 
      [ text $ getString TAP_TO_CALL_OTHER_EMERGENCY_CONTACTS
      , color Color.white900
      , margin $ Margin 16 8 16 0
      ]
      <> FontStyle.tags CTA.TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 10 16
        , gravity CENTER_HORIZONTAL
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                [ height WRAP_CONTENT
                , width $ V $ (EHC.screenWidth unit - 70) / 3
                , gravity CENTER
                , orientation VERTICAL
                , onClick push $ const $ CallContact index
                , margin $ MarginRight 6
                , padding $ PaddingHorizontal 4 4
                ]
                [ ContactCircle.view (ContactCircle.getContactConfig item index false false) (push <<< ContactCircleAction)
                , textView
                    $ [ text item.name
                      , color Color.white900
                      , maxLines 2
                      , ellipsize true
                      , gravity CENTER
                      ]
                    <> FontStyle.body1 CTA.TypoGraphy
                ]
            )
            state.data.emergencyContactsList
        )
    ]

callPoliceView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> STR -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push text' =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      , accessibilityHint "Call Police Button"
      ]
        <> if state.props.showTestDrill then [ alpha 0.6 ] else [ rippleColor Color.rippleShade, onClick push $ const $ if state.props.showCallPolice then CallPolice else ShowPoliceView SafetyActionTileView.OnClick ]
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
        <> FontStyle.subHeading2 CTA.TypoGraphy
    ]

type ImageTextViewConfig
  = { text' :: String
    , isActive :: Boolean
    , useLightTheme :: Boolean
    , useMargin :: Boolean
    , usePadding :: Boolean
    , useFullWidth :: Boolean
    , image :: Maybe String
    }

dialPoliceView :: forall w. ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
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
                    $ [ text $ getString YOUR_CURRENT_LOCATION
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 CTA.TypoGraphy
                , textView
                    $ [ text state.data.currentLocation
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText CTA.TypoGraphy
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
                    $ [ text $ getString YOUR_VEHICLE_INFO
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 CTA.TypoGraphy
                , textView
                    $ [ text $ getVehicleDetails state
                      , color Color.white900
                      , margin $ MarginTop 4
                      ]
                    <> FontStyle.paragraphText CTA.TypoGraphy
                ]
            ]
        , separatorView Color.black500 $ Margin 16 16 16 16
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
                , height $ V 24
                , width $ V 24
                , margin $ MarginRight 5
                ]
            , textView
                $ [ text $ getString POLICE_VIEW_INSTRUCTION
                  , color Color.black500
                  ]
                <> FontStyle.tags CTA.TypoGraphy
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , orientation VERTICAL
        , margin $ MarginBottom 16
        ]
        [ safetyPartnerView CTA.Language
        , callPoliceView state push (DIALING_POLICE_IN_TIME $ show state.props.policeCallTimerValue)
        ]
    ]
