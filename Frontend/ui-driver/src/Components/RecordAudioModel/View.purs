{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.RecordAudioModel.View where

import Prelude
import Common.Types.App (LazyCheck(..))
import Components.RecordAudioModel.Controller (Action(..), RecordAudioModelState)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Style as FontStyle
import JBridge (startLottieProcess, lottieAnimationConfig)
import PrestoDOM (frameLayout, id, linearLayout)
import PrestoDOM.Elements.Elements (imageView, lottieAnimationView, textView)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (background, color, cornerRadius, fontStyle, gravity, height, imageWithFallback, layoutGravity, margin, orientation, padding, text, textSize, visibility, weight, width)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Styles.Colors (black800, blue600, white900) as Color
import Font.Size as FontSize
import Animation (screenAnimationFadeInOut)
import Language.Types (STR(RECORD_VOICE_NOTE))
import Language.Strings (getString)
import Helpers.Utils (fetchImage, FetchImageFrom(..))

view :: forall w. (Action -> Effect Unit) -> RecordAudioModelState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , cornerRadius 16.0
        , background Color.white900
        , padding (Padding 16 24 16 24)
        , margin (MarginHorizontal 16 16)
        , gravity CENTER
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , margin (MarginBottom 16)
            ]
            [ textView
                $ [ text (getString RECORD_VOICE_NOTE)
                  , height WRAP_CONTENT
                  , weight 1.0
                  , color Color.black800
                  , gravity CENTER
                  ]
                <> FontStyle.h2 TypoGraphy
            , imageView
                [ width $ V 28
                , height $ V 28
                , layoutGravity "end"
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_bold"
                , onClick push (const OnClickClose)
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 50
            , background Color.blue600
            , cornerRadius 5.0
            , padding (PaddingHorizontal 18 18)
            ]
            [ frameLayout
                [ width MATCH_PARENT
                , height $ V 50
                , visibility if state.isRecording then VISIBLE else GONE
                ]
                [ lottieAnimationView
                    [ width MATCH_PARENT
                    , height $ V 50
                    , padding (PaddingRight 50)
                    , id (getNewIDWithTag "recordAnimation")
                    , afterRender
                        ( \action -> do
                            void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = "record_audio_animation.json", lottieId = (getNewIDWithTag "recordAnimation"), scaleType = "FIT_CENTER", speed = 1.0 }
                            pure unit
                        )
                        (const NoAction)
                    ]
                , textView
                    $ [ text state.timer
                      , layoutGravity "end"
                      , height $ V 50
                      , gravity CENTER
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            , linearLayout
                [ width MATCH_PARENT
                ]
                ( case state.recordingDone of
                    true ->
                      [ linearLayout
                          [ width MATCH_PARENT
                          , height $ V 50
                          , id (getNewIDWithTag "recordedAudioViewUniqueOne")
                          , orientation VERTICAL
                          , gravity CENTER
                          ]
                          []
                      ]
                    false ->
                      [ frameLayout
                          [ width MATCH_PARENT
                          ]
                          [ imageView
                              [ width WRAP_CONTENT
                              , height $ V 50
                              , visibility if state.isRecording then GONE else VISIBLE
                              , imageWithFallback $ fetchImage FF_ASSET "ny_ic_static_record"
                              , padding (PaddingRight 50)
                              ]
                          , textView
                              $ [ text "00:00"
                                , layoutGravity "end"
                                , height $ V 50
                                , gravity CENTER
                                ]
                              <> FontStyle.body3 TypoGraphy
                          ]
                      ]
                )
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin (MarginTop 20)
            , gravity CENTER
            ]
            [ linearLayout
                [ width $ V 48
                , height $ V 48
                , visibility if state.recordingDone then VISIBLE else GONE
                , id (getNewIDWithTag "actionButtonRecord")
                ]
                []
            , imageView
                [ width $ V 72
                , height $ V 72
                , imageWithFallback $ fetchImage FF_ASSET $ if state.isRecording then "ny_ic_stop_record" else if state.recordingDone then "ny_ic_recording_done" else "ny_ic_start_record"
                , margin (MarginHorizontal 20 20)
                , visibility if state.isUploading then GONE else VISIBLE
                , onClick push
                    ( case state.isRecording of
                        true -> const OnClickStop
                        false -> case state.recordingDone of
                          true -> const OnClickDone
                          false -> const $ OnClickRecord push
                    )
                ]
            , linearLayout
                [ width $ V 72
                , height $ V 72
                , margin (MarginHorizontal 20 20)
                , background Color.blue600
                , cornerRadius 36.0
                , gravity CENTER
                , visibility if state.recordingDone && state.isUploading then VISIBLE else GONE
                ]
                [ lottieAnimationView
                    [ width $ V 48
                    , height $ V 48
                    , id (getNewIDWithTag "audio_recording_done")
                    ]
                ]
            , imageView
                [ width $ V 48
                , height $ V 48
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_cancel_recording"
                , visibility if state.isUploading then INVISIBLE else if state.recordingDone then VISIBLE else GONE
                , onClick push (if state.isUploading then const NoAction else const OnClickRestart)
                ]
            ]
        ]
