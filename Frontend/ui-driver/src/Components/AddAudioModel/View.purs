{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.AddAudioModel.View where

import Prelude
import Components.AddAudioModel.Controller (Action(..), AddAudioModelState, cancelButtonConfig, doneButtonConfig)
import Effect (Effect)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM (linearLayout, text, textView)
import PrestoDOM.Properties (background, color, cornerRadius, fontStyle, gravity, height, id, imageWithFallback, layoutGravity, margin, minHeight, orientation, padding, stroke, textSize, weight, width)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..))
import Styles.Colors (black800, black900, blue900, white900) as Color
import Font.Size (a_16, a_18, a_20) as Font
import Common.Types.App (LazyCheck(..))
import Font.Style (bold)
import PrestoDOM.Elements.Elements (frameLayout, imageView)
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Data.Maybe (Maybe(..), isJust)
import Data.Int (round, toNumber)
import Animation (screenAnimationFadeInOut)
import Language.Strings (getString)
import Language.Types (STR(..))
import JBridge (addMediaFile)
import Helpers.Utils (fetchImage, FetchImageFrom(..))

view :: forall w. (Action -> Effect Unit) -> AddAudioModelState -> PrestoDOM (Effect Unit) w
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
        [ textView
            [ text if (isJust state.audioFile) then (getString ADDED_VOICE_NOTE) else (getString NO_VOICE_NOTE_ADDED)
            , textSize Font.a_20
            , fontStyle $ bold LanguageStyle
            , margin (MarginBottom 16)
            , color Color.black800
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , minHeight 66
            , orientation VERTICAL
            ]
            ( case state.audioFile of
                Just url ->
                  [ frameLayout
                      [ width MATCH_PARENT
                      , orientation HORIZONTAL
                      , stroke "1,#E5E7EB"
                      , cornerRadius 8.0
                      , padding (Padding 16 0 16 0)
                      , margin (MarginBottom 32)
                      , gravity CENTER_VERTICAL
                      ]
                      [ linearLayout
                          [ width $ V $ round (toNumber (screenWidth unit) * 0.6)
                          , height WRAP_CONTENT
                          , afterRender
                              ( \action -> do
                                  pIndex <- addMediaFile (getNewIDWithTag "addAudioFileView") url "-1" "ic_play" "ic_pause" "-1"
                                  pure unit
                              )
                              (const NoAction)
                          , id (getNewIDWithTag "addAudioFileView")
                          , layoutGravity "start"
                          ]
                          []
                      , textView
                          [ width WRAP_CONTENT
                          , height MATCH_PARENT
                          , minHeight 66
                          , text (getString DELETE)
                          , onClick push (const OnClickDelete)
                          , color Color.blue900
                          , textSize Font.a_16
                          , layoutGravity "end"
                          , gravity CENTER_VERTICAL
                          ]
                      ]
                  ]
                _ ->
                  [ linearLayout
                      [ orientation HORIZONTAL
                      , width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , margin (MarginBottom 32)
                      , gravity CENTER_VERTICAL
                      , onClick push (const AddAudio)
                      ]
                      [ imageView
                          [ width $ V 42
                          , height $ V 42
                          , margin (MarginRight 8)
                          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_add_audio"
                          ]
                      , textView
                          [ text (getString ADD_VOICE_NOTE)
                          , textSize Font.a_18
                          , color Color.black900
                          ]
                      ]
                  ]
            )
        , linearLayout
            [ orientation HORIZONTAL
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , margin (MarginTop 16)
            ]
            [ linearLayout
                [ weight 1.0
                , margin (MarginRight 4)
                ]
                [ PrimaryButton.view (push <<< OnClickCancel) (cancelButtonConfig state)
                ]
            , linearLayout
                [ weight 1.0
                , margin (MarginLeft 4)
                ]
                [ PrimaryButton.view (push <<< OnClickDone) (doneButtonConfig state)
                ]
            ]
        ]
