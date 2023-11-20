{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.ViewImageModel.View where

import Prelude (Unit, const, ($), (<>))
import Components.ViewImageModel.Controller (Action(..), ViewImageModelState)
import Effect (Effect)
import PrestoDOM.Types.Core (PrestoDOM)
import Animation (screenAnimationFadeInOut)
import PrestoDOM (linearLayout)
import PrestoDOM.Properties (background, color, gravity, height, id, imageWithFallback, layoutGravity, margin, orientation, padding, text, textSize, weight, width)
import PrestoDOM.Types.DomAttributes
import Styles.Colors (black, white900) as Color
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Elements.Elements (imageView, progressBar, textView)
import Font.Size (a_18) as FontSize
import Font.Style (h3) as FontStyle
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..))
import JBridge (renderBase64Image)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons as EHC

view :: forall w. (Action -> Effect Unit) -> ViewImageModelState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut
  $ linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black
  , onClick push (const BackPressed)
  , onBackPressed push (const BackPressed)
  , orientation VERTICAL
  ][ headerLayout push state
   , linearLayout
   [ width MATCH_PARENT
   , height MATCH_PARENT
   , onClick push (const BackPressed)
   , gravity CENTER
   ][ linearLayout
    [ onClick push (const NoAction)
    , width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , afterRender (\action -> do renderBase64Image state.image (EHC.getNewIDWithTag "view_image_model_image") false "CENTER_CROP") (const NoAction)
    , id (EHC.getNewIDWithTag "view_image_model_image")
    ][ progressBar
       [ width WRAP_CONTENT
       , height WRAP_CONTENT
       ]
     ]
   ]
 ]

-------------------------------------------------- headerLayout --------------------------
headerLayout :: (Action -> Effect Unit) -> ViewImageModelState -> forall w . PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , onClick push (const NoAction)
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , layoutGravity "center_vertical"
        , padding $ Padding 5 16 5 16
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (case state.imageName of
                        Just name -> name
                        _         -> state.imagePreviewText
                     )
              , textSize FontSize.a_18
              , margin $ MarginLeft 20
              , weight 1.0
              , color Color.white900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    ]
