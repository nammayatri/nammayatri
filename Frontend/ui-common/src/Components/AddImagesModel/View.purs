{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.AddImagesModel.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.AddImagesModel.Controller (Action(..), AddImagesModelState, doneButtonConfig)
import Data.Array (length, mapWithIndex)
import Effect (Effect)
import Font.Style (bold, semiBold)
import PrestoDOM (linearLayout, imageView, textView)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (layoutGravity, alpha, background, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageWithFallback, margin, orientation, padding, singleLine, stroke, text, textSize, visibility, weight, width)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Styles.Colors (black800, blue900, white900, blue600) as Color
import Font.Size (a_18, a_20) as Font
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons (getNewIDWithTag)
import Helpers.Utils ( fetchImage, FetchImageFrom(..))
import Animation (screenAnimationFadeInOut)
import JBridge 

view :: forall w . (Action -> Effect Unit) -> AddImagesModelState -> PrestoDOM (Effect Unit) w
view push state =
   screenAnimationFadeInOut
   $ linearLayout
   [ width MATCH_PARENT
   , height WRAP_CONTENT
   , orientation VERTICAL
   , cornerRadius 16.0
   , onBackPressed push (const BackPressed)
   , background Color.white900
   , padding (Padding 16 24 16 24)
   , margin (MarginHorizontal 16 16)
   , gravity CENTER
   ][ 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin (MarginBottom 24)
    ][ textView
     [ text if (length state.images) > 0 then state.addedImagesText else state.noImagesAddedText
     , textSize Font.a_20
     , fontStyle $ bold LanguageStyle
     , height WRAP_CONTENT
     , weight 1.0
     , gravity CENTER
     , margin (MarginLeft 28)
     , fontStyle $ bold LanguageStyle
     , color Color.black800
     ]
     , imageView
     [ width $ V 28
     , height $ V 28
     , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_bold"
     , onClick push (const OnClickCancel)
     ]
    ],
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ] (mapWithIndex (\index image ->
                      linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , stroke "1,#E5E7EB"
                      , cornerRadius 8.0
                      , margin (MarginBottom 16)
                      , gravity CENTER_VERTICAL
                      , afterRender (\action -> do
                                      renderBase64ImageFile image.image (getNewIDWithTag "add_image_component_image" <> (show index)) false "CENTER_CROP"
                                    ) (const NoAction)
                      , padding (Padding 16 16 16 16)
                      ][ linearLayout
                       [ width $ V 48
                       , height $ V 48
                       , cornerRadius 4.0
                       , margin (MarginRight 16)
                       , gravity CENTER_VERTICAL
                       , onClick push (case state.isLoading of
                          false -> (const $ OnClickView image.image image.imageName)
                          true  -> (const NoAction)
                        )
                       , id (getNewIDWithTag "add_image_component_image" <> (show index))
                       ][],
                       textView
                        [ text image.imageName
                        , singleLine true
                        , width $ V 100
                        , textSize Font.a_18
                       , gravity CENTER_VERTICAL
                        , fontStyle $ semiBold LanguageStyle
                        , onClick push (case state.isLoading of
                                        false -> (const $ OnClickView image.image image.imageName)
                                        true  -> (const NoAction)
                                       )
                        , alpha if state.isLoading then 0.3 else 1.0
                        , ellipsize true]
                      , linearLayout [
                          height $ V 0,
                          width $ V 0,
                          weight 1.0
                        ][]
                      , imageView
                          [ width $ V 24
                          , height $ V 24
                          , margin (MarginRight 6)
                          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_round_cross"
                          , onClick push (case state.isLoading of
                                          false -> (const $ OnClickDelete index)
                                          true  -> (const NoAction)
                                          ) 
                          , gravity RIGHT
                          , layoutGravity "center_vertical"
                          ]
                      ]
                    ) state.images)
      , linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginBottom 16)
      , onClick push (if state.isLoading then pure NoAction else pure AddImage)
      , gravity CENTER
      , alpha if state.isLoading then 0.5 else 1.0
      , visibility if length state.images >= 3 || state.isLoading then GONE else VISIBLE
      , background Color.blue600
      , padding (Padding 12 16 16 12)
      , cornerRadius 10.0
      ][ imageView
       [ width $ V 14
       , height $ V 14
       , margin (MarginRight 8)
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_add_image"
       ]
       , textView
       [ text state.addImageText
       , textSize Font.a_18
       , color Color.blue900
       ]
      ]
      , linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginTop 16)
      ][ 
        linearLayout
       [ weight 1.0
       , width MATCH_PARENT
       ][ PrimaryButton.view (push <<< OnClickDone) (doneButtonConfig state)
       ]
      ]
    ]