{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.TutorialModal.View where

import Prelude(Unit, ($), const, (<>))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, clickable, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, width, textView, fontStyle, text, color, weight, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config (translateYAnimConfig)
import Animation (translateYAnim)
import Components.TutorialModal.Controller (Action(..), State)
import Styles.Colors as Color
import PrestoDOM.Properties(cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Language.Types (STR(..))
import Language.Strings (getString)
import Font.Style as FontStyle
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity BOTTOM
    , orientation VERTICAL
    , background Color.black9000
    , onBackPressed push (const OnCloseClick)
    , clickable true
    ][  PrestoAnim.animationSet [
            translateYAnim translateYAnimConfig
        ] $ 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (PaddingHorizontal 20 20)
        , cornerRadii $ Corners 20.0 true true false false
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ height $ V 18
                , width $ V 18
                , margin (MarginTop 20)
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                , clickable true
                , onClick push (const OnCloseClick)
                ]
            ]
        ,   imageView
            [ height $ V 290
            , width WRAP_CONTENT
            , imageWithFallback state.imageUrl
            , weight 1.0
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding (PaddingVertical 40 40)
            , orientation VERTICAL
            , gravity LEFT
            ][  textView
                ([ height WRAP_CONTENT
                , width MATCH_PARENT
                , text (getString STILL_HAVE_SOME_DOUBT)
                , color Color.black700
                , margin $ MarginBottom 12
                ] <> FontStyle.body3 TypoGraphy)
            ,  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity LEFT
                , orientation HORIZONTAL
                , onClick push (const CallSupport)
                ][ imageView
                [ imageWithFallback $ fetchImage FF_ASSET  "ny_ic_support"
                , height $ V 17
                , width $ V 20
                , margin $ (Margin 0 0 7 27)
                ]
                , textView
                    ([ text (getString CALL_SUPPORT_CENTER)
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy)
                ]
            ,  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity LEFT
                , orientation HORIZONTAL
                , onClick push (const Logout)
                ][ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_logout_grey"
                , height $ V 17
                , width $ V 20
                , margin $ MarginRight 7
                ]
                , textView
                    ([ text (getString LOGOUT)
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy)
                ]
            ]
        ]
    ]