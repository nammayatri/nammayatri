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
import Components.AddAudioModel.Controller (Action(..), AddAudioModelState, donePrimaryButtonConfig)
import Effect (Effect)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM (linearLayout, text, textView, imageView)
import PrestoDOM.Properties (alpha, visibility, background, color, cornerRadius, fontStyle, gravity, height, id, imageWithFallback, layoutGravity, margin, orientation, padding, stroke, textSize, weight, width)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Styles.Colors (black800, blue900, white900, blue600) as Color
import Font.Size (a_18, a_20) as Font
import Common.Types.App (LazyCheck(..))
import Font.Style (bold)
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Animation (screenAnimationFadeInOut)
import JBridge (addMediaFile)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Styles.Colors as Color
import Font.Style as FontStyle
import Mobility.Prelude (boolToVisibility)

view :: forall w. (Action -> Effect Unit) -> AddAudioModelState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , cornerRadius 16.0
        , background Color.white900
        , padding $ Padding 16 24 16 24
        , margin $ MarginHorizontal 16 16
        , gravity CENTER
        ]
        [ headerView push state
        , bodyView push state
        , PrimaryButton.view (push <<< OnClickDone) (donePrimaryButtonConfig state)
        ]

headerView :: forall w. (Action -> Effect Unit) -> AddAudioModelState -> PrestoDOM (Effect Unit) w 
headerView push state = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginBottom 24
        ]
        [ textView
            $ [ text if isJust state.audioFile then state.addedVoiceNoteText else state.noVoiceNoteAddedText
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , weight 1.0
                , gravity CENTER
                , margin $ MarginLeft 28
                , color Color.black800
                ]
            <> FontStyle.body8 TypoGraphy
        , imageView
            [ width $ V 28
            , height $ V 28
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_bold"
            , onClick push $ const OnClickCross
            ]
        ]

bodyView :: forall w. (Action -> Effect Unit) -> AddAudioModelState -> PrestoDOM (Effect Unit) w
bodyView push state = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , stroke $ "1," <> Color.blue800
        , cornerRadius 8.0
        , padding $ PaddingHorizontal 16 16
        , gravity CENTER
        , background Color.blue600
        , margin $ MarginBottom 24
        ]
        [ linearLayout
            [ orientation HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , onClick push $ const AddAudio
            , gravity CENTER
            , alpha 1.0
            , visibility $ boolToVisibility $ not $  isJust state.audioFile
            , background Color.blue600
            , padding $ Padding 12 16 12 16
            , cornerRadius 10.0
            ]
            [ imageView
                [ width $ V 14
                , height $ V 14
                , margin $ MarginRight 8
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_add_unfilled"
                ]
            , textView
                $ [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text state.addVoiceNoteText
                    , color Color.blue900
                    ]
                <> FontStyle.body1 TypoGraphy
            ]
        , linearLayout
            [ height $ V 64
            , width WRAP_CONTENT
            , padding $ PaddingVertical 8 8
            , gravity CENTER_VERTICAL
            , visibility $ boolToVisibility $ isJust state.audioFile
            ]
            [ linearLayout
                [ width $ V $ (screenWidth unit) - 140
                , height MATCH_PARENT
                , afterRender
                    ( \action -> do
                        pIndex <- addMediaFile (getNewIDWithTag "addAudioFileView") (fromMaybe "" state.audioFile) "-1" "ic_play" "ic_pause" "-1"
                        pure unit
                    )
                    (const NoAction)
                , id $ getNewIDWithTag "addAudioFileView"
                , layoutGravity "start"
                ]
                []
            , imageView
                [ width $ V 24
                , height $ V 24
                , margin $ MarginHorizontal 10 6
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_cross_red"
                , onClick push $ const $ OnClickDelete
                , gravity RIGHT
                , layoutGravity "center_vertical"
                ]
            ]
        ]