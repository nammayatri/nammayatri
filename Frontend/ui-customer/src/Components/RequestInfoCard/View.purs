{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RequestInfoCard.View where

import Common.Types.App

import Common.Types.App (LazyCheck(..))
import Components.RequestInfoCard.Controller (Action(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((*), Unit, ($), const, (/), unit, (-))
import Prelude ((<>))
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Padding(..), Margin(..), Length(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, onClick, color, background, lineHeight, cornerRadius, weight, onBackPressed, imageWithFallback)
import Screens.Types (HomeScreenState)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> HomeScreenState ->PrestoDOM (Effect Unit) w 
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 0 16 0)
  , gravity CENTER
  , background Color.black9000
  , onClick push $ const BackPressed
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , background Color.white900
     , cornerRadius 16.0
     , onClick push $ const NoAction
     ][ 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][ 
            linearLayout
            [ width (V ((((screenWidth unit)/3) * 2) - 27))
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][
                textView$
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 16 24 0 0)
                , text  (getString CHOOSE_BETWEEN_MULTIPLE_RIDES)
                , color Color.black800
                ] <> FontStyle.subHeading1 LanguageStyle
                , textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 16 16 0 0)
                , text (getString ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE)
                , color Color.black700
                ] <> FontStyle.paragraphText LanguageStyle
            ]
            , linearLayout
              [ height WRAP_CONTENT
              ,   weight 1.0
              ][]
            , imageView
              [ width $ V 116
              , height $ V 122
              , imageWithFallback $ "ny_ic_select_offer," <> (getAssetStoreLink FunctionCall) <> "ny_ic_select_offer.png"
              ]
        ], textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , color state.data.config.alertDialogPrimaryColor
            , gravity CENTER
            , text (getString GOT_IT)
            , padding (Padding 0 28 0 20)
            , onClick push $ const Close
            ] <> FontStyle.subHeading1 LanguageStyle
     ]

  ]