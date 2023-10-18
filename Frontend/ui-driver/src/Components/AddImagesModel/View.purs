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

import Common.Types.App (LazyCheck(LanguageStyle))
import Components.AddImagesModel.Controller (Action(..), AddImagesModelState, cancelButtonConfig, doneButtonConfig)
import Data.Array (length, mapWithIndex)
import Effect (Effect)
import Font.Style (bold, semiBold)
import PrestoDOM (linearLayout)
import PrestoDOM.Elements.Elements (imageView, textView)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (alpha, background, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageWithFallback, margin, orientation, padding, singleLine, stroke, text, textSize, visibility, weight, width)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Styles.Colors (black900, blue900, white900) as Color
import Font.Size (a_16, a_18, a_20) as Font
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons (getNewIDWithTag)
import Helpers.Utils (renderBase64ImageFile, fetchImage, FetchImageFrom(..))
import Animation (screenAnimationFadeInOut)
import Language.Strings (getString)
import Language.Types (STR(..))

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
   ][ textView
    [ text if (length state.images) > 0 then (getString ADDED_IMAGES) else (getString NO_IMAGES_ADDED)
    , textSize Font.a_20
    , fontStyle $ bold LanguageStyle
    , margin (MarginBottom 16)
    ]
    , linearLayout
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
                       , gravity CENTER
                       , id (getNewIDWithTag "add_image_component_image" <> (show index))
                       ][]
                       , linearLayout
                       [ width MATCH_PARENT
                       , height WRAP_CONTENT
                       , orientation VERTICAL
                       ][ textView
                        [ text image.imageName
                        , singleLine true
                        , textSize Font.a_18
                        , margin (MarginBottom 8)
                        , fontStyle $ semiBold LanguageStyle
                        , onClick push (case state.isLoading of
                                        false -> (const $ OnClickView image.image image.imageName)
                                        true  -> (const NoAction)
                                       )
                        , alpha if state.isLoading then 0.3 else 1.0
                        , ellipsize true
                        ]
                        , linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , orientation HORIZONTAL
                        ][ textView
                         [ text (getString VIEW)
                         , textSize Font.a_16
                         , onClick push (case state.isLoading of
                                         false -> (const $ OnClickView image.image image.imageName)
                                         true  -> (const NoAction)
                                        )
                         , color Color.blue900
                         , margin (MarginRight 18)
                         , alpha if state.isLoading then 0.3 else 1.0
                         ]
                         , textView
                         [ text (getString DELETE)
                         , textSize Font.a_16
                         , onClick push (case state.isLoading of
                                         false -> (const $ OnClickDelete index)
                                         true  -> (const NoAction)
                                        )
                         , color Color.blue900
                         , alpha if state.isLoading then 0.3 else 1.0
                         ]
                        ]
                       ]
                      ]
                    ) state.images)
      , linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginBottom 16)
      , onClick push (if state.isLoading then pure NoAction else pure AddImage)
      , gravity CENTER_VERTICAL
      , alpha (if state.isLoading then 0.5 else 1.0)
      , visibility if length state.images >= 3 || state.isLoading then GONE else VISIBLE
      ][ imageView
       [ width $ V 42
       , height $ V 42
       , margin (MarginRight 8)
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_add_image"
       ]
       , textView
       [ text if (length state.images) > 0 then (getString ADD_ANOTHER) else (getString ADD_IMAGE)
       , textSize Font.a_18
       , color Color.black900
       ]
      ]
      , linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginTop 16)
      ][ linearLayout
       [ weight 1.0
       , margin (MarginRight 4)
       , width MATCH_PARENT
       ][ PrimaryButton.view (push <<< OnClickCancel) (cancelButtonConfig state)
       ]
       , linearLayout
       [ weight 1.0
       , margin (MarginLeft 4)
       , width MATCH_PARENT
       ][ PrimaryButton.view (push <<< OnClickDone) (doneButtonConfig state)
       ]
      ]
    ]