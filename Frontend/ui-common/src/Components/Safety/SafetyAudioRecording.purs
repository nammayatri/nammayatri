module Components.Safety.SafetyAudioRecording where

import PrestoDOM
import Prelude
import Data.Maybe as Mb
import Common.Types.App (RecordingState(..), LazyCheck(..))
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>),(==), (||), (&&), (/), (*), (/=), (+), (<<<), unit, map, (-), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml,imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight,fontStyle, scrollView, maxLines, singleLine, stroke, horizontalScrollView, relativeLayout)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (os, screenWidth, screenHeight)
import Mobility.Prelude (boolToVisibility, boolToInvisibility)
import Timers (startTimer)
import Components.Safety.Utils as SU
import Mobility.Prelude as MP
import Animation (fadeIn)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state = 
  if state.isAudioRecordingActive then 
     PrestoAnim.animationSet
      [ fadeIn true
      ]
      $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.black700
        , background Color.black712
        , cornerRadius 12.0
        , orientation VERTICAL
        , margin $ Margin 16 16 16 16
        , padding $ Padding 16 16 16 (if state.audioRecordingStatus == RECORDED then 0 else 16)
        , visibility $ boolToVisibility state.isAudioRecordingActive 
        ]$ [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            ][ imageView
                [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_microphone_white"
                , height $ V 24
                , width $ V 24
                ]
              , textView
                $ [ text $ getStringWithoutNewLine $ case state.audioRecordingStatus of
                         RECORDED -> RECORDED_AUDIO 
                         RECORDING -> RECORDING_AUDIO
                         _ -> RECORD_AUDIO
                  , color Color.white900
                  ]
                <> FontStyle.subHeading1 TypoGraphy
              , SU.layoutWithWeight
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
                , background $ if state.audioRecordingStatus == RECORDING then Color.blue800 else Color.black900
                , padding $ Padding 12 12 12 12
                , margin $ MarginTop 12
                , cornerRadius 4.0
                , visibility $ boolToVisibility $ state.audioRecordingStatus /= RECORDED
                ][ imageView
                    [ width $ V 24
                    , height $ V 24
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.audioRecordingStatus == RECORDING then "ny_ic_stop_record" else  "ny_ic_play_black_white"
                    , visibility $ boolToVisibility $ state.audioRecordingStatus /= RECORDED
                    , onClick push
                        ( case state.audioRecordingStatus of
                            NOT_RECORDING -> const StartRecord
                            RECORDING -> const StopRecord
                            _ -> const NoAction
                        )
                    ]
                  , imageView
                    [ weight 1.0
                    , height $ V 50
                    , margin $ MarginHorizontal 8 8
                    , visibility $ boolToVisibility $ state.audioRecordingStatus == NOT_RECORDING
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_static_record"
                    ]
                  , linearLayout
                    [ weight 1.0
                    , height $ V 50
                    , visibility $ boolToVisibility $ state.audioRecordingStatus == RECORDING
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
                [ text state.recordingTimer
                , height $ V 50
                , gravity CENTER
                , color Color.white900
                ] <> FontStyle.body1 TypoGraphy
            ]
      ,  linearLayout
              [ width MATCH_PARENT
              , height $ V 50
              , gravity CENTER_VERTICAL
              , visibility $ boolToInvisibility $ state.audioRecordingStatus == RECORDED
              , background Color.black900
              , margin $ MarginTop 12
              , padding $ PaddingHorizontal 12 12
              , cornerRadius 4.0
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
      , PrimaryButton.view (push <<< ShareAudio) state.shareAudioButtonConfig
      ]
    else MP.noView

data Action = StartRecord | StopRecord | CancelAudioRecording | ShareAudio PrimaryButton.Action | NoAction

type Config = {
    isAudioRecordingActive :: Boolean, 
    audioRecordingStatus :: RecordingState, 
    recordingTimer :: String,
    shareAudioButtonConfig :: PrimaryButton.Config
}


