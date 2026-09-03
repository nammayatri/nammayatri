{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ReferralStepsView.View where

import Prelude
import PrestoDOM (textFromHtml, scrollView, frameLayout, shimmerFrameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, singleLine, maxLines, minHeight)
import Data.Array (mapWithIndex, length)
import Language.Strings (getString, getVarString)
import Helpers.Utils 
import Styles.Colors as Color
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Components.ReferralStepsView.Controller
import Mobility.Prelude
import Font.Style as FontStyle
import Common.Types.App
import Language.Types (STR(..))
import Engineering.Helpers.Commons as EHC

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , background Color.black9000
  , clickable true
  , onClick push $ const $ GoBack
  ][relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , cornerRadius 16.0
    , clickable true
    , margin $ MarginHorizontal 16 16
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][  imageView
            [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_gift_boxes"
            , height $ V 122
            , width $ V 118
            ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ][textView $ 
            [ text config.heading
            , width $ V $ (EHC.screenWidth unit) - 150
            , color Color.black800
            , padding $ PaddingLeft 16
            , margin $ MarginVertical 24 16
            ] <> FontStyle.body25 TypoGraphy
          , textView $ 
            [ text $ (getString STEPS) <> ":"
            , color Color.black650
            , padding $ PaddingLeft 16
            , margin $ MarginBottom 8
            ] <> FontStyle.body2 TypoGraphy
          , stagesView push config
          , linearLayout
            [ height $ V 48
            , width MATCH_PARENT
            , margin $ MarginTop 20
            , gravity CENTER
            , onClick push $ const $ GoBack
            ][ textView $ 
              [ text $ getString GOT_IT
              , color Color.blue800
              ] <> FontStyle.subHeading3 TypoGraphy
            ]
          ]
       ]
    ]
  ]

stagesView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
stagesView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , padding $ PaddingLeft 16
  , orientation VERTICAL
  ] $ mapWithIndex
      ( \index item ->
          linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          ][linearLayout
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ][ linearLayout
              [ height $ V 30
              , width $ V 30
              , cornerRadius 15.0 
              , gravity CENTER
              , background item.background
              , orientation VERTICAL
              , margin $ MarginRight 12
              ][ imageView 
                [ height $ V 16
                , width $ V 16
                , imageWithFallback item.icon
                ]
              ]
            , linearLayout
              [ height MATCH_PARENT
              , width $ V 30
              , gravity CENTER
              , visibility $ boolToVisibility $ (index /= (length config.referralSteps) - 1)
              ][linearLayout
                [ height MATCH_PARENT
                , width $ V if config.highlightTitle then 1 else 2
                , gravity CENTER
                , background $ if item.status == Done then Color.green900 else Color.grey900
                ][]
              ]
            ]
          , textView $ 
            [ text $ item.title 
            , minHeight 30
            , gravity CENTER_VERTICAL
            , padding $ Padding 0 0 16 3
            , margin $ MarginBottom 16
            , color $ if item.status == InProgress || config.highlightTitle then Color.black800 else Color.black650
            ] <> FontStyle.body20 TypoGraphy
        ]
      )
      config.referralSteps
