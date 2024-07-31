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
import Animation (screenAnimationFadeInOut, fadeIn)
import Prelude (Unit, const, discard, not, pure, unit, void, ($), (&&), (<<<), (<>), (==), (<#>), map, (/), (-), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alignParentBottom, alpha, background, color, cornerRadius, gravity, height, id, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, relativeLayout, rippleColor, scrollView, stroke, text, textView, visibility, weight, width, accessibilityHint, maxLines, ellipsize)
import Screens.NammaSafetyFlow.ComponentConfig (cancelSOSBtnConfig, shareAudioButtonConfig)
import Screens.NammaSafetyFlow.Components.HelperViews (layoutWithWeight, safetyPartnerView, separatorView, emptyTextView)
import Common.Types.App (LazyCheck(..))
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
import Language.Strings (getString)
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

screen :: ST.NammaSafetyScreenState -> Screen Action ST.NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SosActiveScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  lift $ lift $ doAff do liftEffect $ push $ PlaceCall
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
  linearLayout
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
    (Header.config Language)
      { useLightColor = true
      , title = SU.getStringWithoutNewLine $ if not state.props.showCallPolice then EMERGENCY_SOS else CALL_POLICE
      , learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY
      , showLearnMore = false
      }

------------------------------------- dashboardView -----------------------------------
sosActiveView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
sosActiveView state push =
  scrollView
  [ width MATCH_PARENT
  , weight 1.0
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ SosButtonAndDescription.view (push <<< SosButtonAndDescriptionAction) $ emergencySosConfig state 
          
        , emergencyContactsView state push
        , audioRecordingAnimationView push state
        , sosActionTilesView state push
        , layoutWithWeight
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
  ]
  

emergencySosConfig :: ST.NammaSafetyScreenState -> SosButtonAndDescription.Config
emergencySosConfig state =
  SosButtonAndDescription.config
  { 
    sosDescription = [ measureViewConfig {text' = getString SAFETY_TEAM_CALLBACK_REQUESTED}
                      , measureViewConfig {text' = getString EMERGENCY_CONTACTS_NOTIFIED}
                      ] <> case contactName of
                          Mb.Just name -> [ measureViewConfig {text' = getString CALL_PLACED <> ": " <> name} ]
                          Mb.Nothing -> []
  , descriptionText = getString EMERGENCY_SOS_ACTIVATED
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
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , visibility $ boolToVisibility $ not state.props.isAudioRecordingActive 
    ]
    [ safetyActionRow actionsRow1 push
    , safetyActionRow actionsRow2 push
    ]

  where
    actionsRow1 =
        [ { text: SU.getStringWithoutNewLine CALL_POLICE , image: "ny_ic_police" , backgroundColor: Color.redOpacity20, strokeColor: Color.redOpacity30, push : push <<< ShowPoliceView }
        , { text: SU.getStringWithoutNewLine RECORD_AUDIO, image: "ny_ic_microphone_white", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< RecordAudio }
        ]

    actionsRow2 =
        [ { text: getString SIREN, image: sirenImage, backgroundColor: sirenTileBackgroundColor, strokeColor: sirenTileStrokeColor, push : push <<< ToggleSiren }
        , { text: SU.getStringWithoutNewLine CALL_SAFETY_TEAM, image: "ny_ic_police_alert", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< CallSafetyTeam }
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
                    SafetyActionTileView.view item.image item.text item.push item.backgroundColor item.strokeColor (V $ (EHC.screenWidth unit - 44) / 2) true
                )
                actionItems
            )
        ]
      
    sirenImage = if state.props.triggerSiren then "ny_ic_full_volume" else "ny_ic_no_volume"
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
                <> FontStyle.h3 TypoGraphy
            , textView
                $ [ text desc
                  , color Color.white900
                  , margin $ MarginTop 8
                  , gravity CENTER
                  ]
                <> FontStyle.body1 TypoGraphy
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
                                void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig { rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/ny_ic_sos_active.json", lottieId = EHC.getNewIDWithTag "SafetylottieAnimationView", scaleType = "FIT_CENTER", repeat = true }
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

audioRecordingAnimationView :: forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
audioRecordingAnimationView push state = 
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $
     linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.black700
        , background Color.black712
        , cornerRadius 12.0
        , orientation VERTICAL
        , margin $ Margin 16 16 16 16
        , padding $ Padding 16 16 16 16
        , visibility $ boolToVisibility state.props.isAudioRecordingActive 
        ]$ [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            ][ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_microphone_white"
                , height $ V 24
                , width $ V 24
                ]
              , textView
                $ [ text $ SU.getStringWithoutNewLine $ case state.props.audioRecordingStatus of
                           ST.RECORDED -> RECORDED_AUDIO 
                           ST.RECORDING -> RECORDING_AUDIO
                           _ -> RECORD_AUDIO
                  , color Color.white900
                  ]
                <> FontStyle.subHeading1 TypoGraphy
              , layoutWithWeight
              , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_cross_white"
                , height $ V 24
                , width $ V 24
                , onClick push $ const CancelAudioRecording
                ]
            ]

          , relativeLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            ][ linearLayout
                [ width MATCH_PARENT
                , height $ V 50
                , gravity CENTER_VERTICAL
                , background $ if state.props.audioRecordingStatus == ST.RECORDING then Color.blue800 else Color.black900
                , padding $ Padding 12 12 12 12
                , margin $ MarginTop 12
                , cornerRadius 4.0
                , visibility $ boolToVisibility $ state.props.audioRecordingStatus /= ST.RECORDED
                ][ imageView
                    [ width $ V 24
                    , height $ V 24
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.audioRecordingStatus == ST.RECORDING then "ny_ic_stop_record" else  "ny_ic_play_black_white"
                    , visibility $ boolToVisibility $ state.props.audioRecordingStatus /= ST.RECORDED
                    , onClick push
                        ( case state.props.audioRecordingStatus of
                            ST.NOT_RECORDING -> const StartRecord
                            ST.RECORDING -> const StopRecord
                            _ -> const NoAction
                        )
                    ]
                  , imageView
                    [ weight 1.0
                    , height $ V 50
                    , margin $ MarginHorizontal 8 8
                    , visibility $ boolToVisibility $ state.props.audioRecordingStatus == ST.NOT_RECORDING
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_static_record"
                    ]
                  , linearLayout
                    [ weight 1.0
                    , height $ V 50
                    , visibility $ boolToVisibility $ state.props.audioRecordingStatus == ST.RECORDING
                    ][ lottieAnimationView
                        [ width MATCH_PARENT
                        , padding $ PaddingRight 8
                        , height $ V 50
                        , id $ EHC.getNewIDWithTag "recordAnimation"
                        , afterRender
                            ( \action -> do
                                void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig { rawJson = "record_audio_animation.json", lottieId = (EHC.getNewIDWithTag "recordAnimation"), scaleType = "FIT_CENTER", speed = 1.0 }
                                pure unit
                            )
                            (const NoAction)
                        ]

                    ]
            , textView $
                [ text state.props.recordingTimer
                , height $ V 50
                , gravity CENTER
                , color Color.white900
                ] <> FontStyle.body1 TypoGraphy
            ]
      ,  linearLayout
              [ width MATCH_PARENT
              , height $ V 50
              , gravity CENTER_VERTICAL
              ][
                linearLayout
              [ width $ V 24
              , height $ V 50
              , id $ EHC.getNewIDWithTag "recordedAudiobtn"
              ][
                
              ]
              ,linearLayout
              [ weight 1.0
              , height $ V 50
              , padding $ PaddingHorizontal 8 8
              , id $ EHC.getNewIDWithTag "recordedAudioViewUniqueOne"
              ][
                
              ]
              ,linearLayout
              [ width WRAP_CONTENT
              , height $ V 50
              , id $ EHC.getNewIDWithTag "recordedAudiotimer"
              ][
                
              ]
              ]
            ]
      , PrimaryButton.view (push <<< ShareAudio) (shareAudioButtonConfig state)
      -- , textView $
      --   [ text $ getString SHARE_WITH_SAFETY_TEAM
      --   , color Color.blue800
      --   , margin $ MarginTop 16
      --   , gravity CENTER
      --   , width MATCH_PARENT
      --   ] <> FontStyle.body1 TypoGraphy
      ]
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
            <> FontStyle.paragraphText TypoGraphy
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
    , padding $ Padding 16 14 16 14
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
    ]
    [ textView $ 
      [ text $ getString TAP_TO_CALL_OTHER_EMERGENCY_CONTACTS
      , color Color.white900
      ]
      <> FontStyle.tags TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 16
        , gravity CENTER
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  [ height WRAP_CONTENT
                  , weight 1.0
                  , gravity CENTER
                  , orientation VERTICAL
                  , onClick push $ const $ CallContact index
                  ]
                  [ ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactCircleAction)
                  , textView
                      $ [ text item.name
                        , color Color.white900
                        , maxLines 2
                        , ellipsize true
                        ]
                      <> FontStyle.body1 TypoGraphy
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
        <> FontStyle.subHeading2 TypoGraphy
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
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text state.data.currentLocation
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
                    $ [ text $ getString YOUR_VEHICLE_INFO
                      , color Color.white900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text $ getVehicleDetails state
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
                <> FontStyle.tags TypoGraphy
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , orientation VERTICAL
        , margin $ MarginBottom 16
        ]
        [ safetyPartnerView Language
        , callPoliceView state push DIAL_NOW
        ]
    ]
