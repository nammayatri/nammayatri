{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EditAadhaarDetailsScreen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, visibility, onBackPressed, afterRender, imageWithFallback)
import Screens.EditAadhaarDetailsScreen.Controller (Action(..), ScreenOutput, eval, getTitleFromList)
import Screens.EditAadhaarDetailsScreen.ScreenData (viewsItemList, ListOptions(..))
import Screens.Types as ST
import Prelude (Unit, const, map, ($), (==))
import Language.Strings (getString)
import Animation as Anim
import Language.Types (STR(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Effect (Effect)
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

screen :: ST.EditAadhaarDetailsScreenState -> Screen Action ST.EditAadhaarDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EditAadhaarDetailsScreen"
  , globalEvents : []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> ST.EditAadhaarDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , background Color.white900
    , afterRender push (const AfterRender)
    ][  linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ][ frameLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ][ editAadhaarDetailsScreen state push]
        ]
    ]


editAadhaarDetailsScreen :: ST.EditAadhaarDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editAadhaarDetailsScreen state push = 
 linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginTop 10)
    ][ headerLayout state push (getString AADHAR_DETAILS),
       aadhaarDetailsView state push
    ]

headerLayout :: ST.EditAadhaarDetailsScreenState -> (Action -> Effect Unit) -> String -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push heading =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , margin (MarginTop 5)
 , layoutGravity "center_vertical"
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding (Padding 5 5 5 5)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_black"
        , layoutGravity "center_vertical"
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        , onClick push (const BackPressed)
        ],
        textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text heading
        , margin (MarginLeft 20)
        , color Color.black
        , weight 1.0
        ] <> FontStyle.h3 LanguageStyle,
        textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString EDIT)
        , margin (MarginRight 10)
        , color Color.blueBtn
        , gravity RIGHT
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , visibility if state.props.isInEditAadharDetailsScreen then GONE else VISIBLE
        , onClick push (const ToggleScreenMode)
        ]
    ],
    linearLayout
    [ width MATCH_PARENT
    , height $ V 1 
    , background Color.black800
    , alpha 0.1
    , margin (MarginTop 10)
    ][]
 ]

aadhaarDetailsView :: ST.EditAadhaarDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
aadhaarDetailsView state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (PaddingBottom 5)
    , margin (MarginTop 12)
    ] (map(\optionItem ->
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              , margin (MarginTop 20)
              ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , gravity CENTER_VERTICAL
                , padding (PaddingHorizontal 15 15)
                ][  textView
                    [ height WRAP_CONTENT
                    , text (getTitleFromList optionItem.title)
                    , color Color.black800
                    , alpha 0.8
                    ],
                    linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , margin (MarginTop 10)
                    , orientation HORIZONTAL
                    ][ textView $
                       [ width WRAP_CONTENT
                       , height WRAP_CONTENT
                       , text optionItem.value
                       , color Color.black800
                       ] <> FontStyle.subHeading1 LanguageStyle,
                       textView $
                       [ width WRAP_CONTENT
                       , height WRAP_CONTENT
                       , margin (MarginLeft 10)
                       , text (getString PREVIEW)
                       , color Color.blueBtn
                       , visibility if (optionItem.title == AADHAAR_NUMBER) then GONE else VISIBLE
                       ] <> FontStyle.subHeading1 LanguageStyle
                    ]
                ]
              , if(optionItem.title == IMAGE_BACK_SIDE ) then dummyTextView else horizontalLineView
            ]
          ) viewsItemList
    )
 ]


horizontalLineView :: forall w . PrestoDOM (Effect Unit) w
horizontalLineView = 
 linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.black500
  , margin (Margin 15 20 15 0)
  , alpha 0.5
  ][]


dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView = 
 textView
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 ]