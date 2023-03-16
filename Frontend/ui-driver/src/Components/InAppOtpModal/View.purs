{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InAppOtpModal.View where

import Common.Types.App
import Components.InAppOtpModal.Controller

import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Array (mapWithIndex)
import Data.String (take, drop, length)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (/), (<>), (==), (||), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), imageUrl, imageView, linearLayout, onBackPressed, onClick, textView, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (background, backgroundDrawable, clickable, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, padding, stroke, text, textSize, weight, width, visibility)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> InAppOtpModalState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout 
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
    , gravity BOTTOM
    -- , onBackPressed push (const BackPressed)
    ][
     PrestoAnim.animationSet [
        translateYAnim translateYAnimConfig
      ] $ 
        linearLayout 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , background Color.white900
        , gravity CENTER
        ][  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , margin (MarginTop 10)
            ][  linearLayout 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , margin (Margin 20 20 20 20 )
                , gravity CENTER_VERTICAL
                ][  imageView
                    [ width (V 30)
                    , height (V 30)
                    , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
                    , onClick push (const BackPressed)
                    , padding (Padding 5 5 5 5)
                    ]
                  , textView (
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text (getString ENTER_OTP)
                    , color Color.greyTextColor
                    , margin (MarginLeft 16)
                    ] <> FontStyle.subHeading1 TypoGraphy
                    )
                ]
              , textBoxes push state
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin (Margin 20 0 20 30)
                , orientation VERTICAL
                , gravity CENTER
                ][  textView (
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color Color.black600
                    , text (getString PLEASE_ASK_RIDER_FOR_THE_OTP)
                    ] <> FontStyle.body1 TypoGraphy
                    )
                  , textView (
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , margin (MarginTop 8)
                    , text if state.otpIncorrect then (getString ENTERED_WRONG_OTP) else if state.otpAttemptsExceeded then (getString YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN) else ""
                    , color Color.red
                    ] <> FontStyle.body1 TypoGraphy
                    )
                ]
            ]
          , keyboard push state
        ]
    ]

textBoxes :: forall w . (Action -> Effect Unit) -> InAppOtpModalState -> PrestoDOM (Effect Unit) w
textBoxes push state = 
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin (Margin 40 20 40 20)
  , clickable false
  ](mapWithIndex (\index item -> 
      textView
      [ width (V 48)
      , height (V 56)
      , color Color.greyTextColor
      , text ( take 1 (drop index state.text) )
      , textSize state.fontSize
      , fontStyle $ FontStyle.bold LanguageStyle
      , weight 1.0
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> if (state.otpIncorrect || state.otpAttemptsExceeded ) then Color.textDanger else if state.focusIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      , margin (Margin ((screenWidth unit)/30) 0 ((screenWidth unit)/30) 0)
      , onClick push (const (OnclickTextBox index))
      ]) [1,2,3,4]
  )

keyboard :: forall w . (Action -> Effect Unit) -> InAppOtpModalState -> PrestoDOM (Effect Unit) w
keyboard push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding (Padding 0 5 0 20)
  , gravity CENTER
  , background Color.grey800
  ] (map (\(item) -> 
    linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     , margin (Margin 4 0 4 0)
     , gravity CENTER
     ] (mapWithIndex (\index key -> 
       linearLayout
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       , gravity CENTER
       , weight 1.0
       , backgroundDrawable "button"
       ][  if (key == "back" || key == "done") then
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , background if key == "back" then Color.lightGrey else if (length state.text) == 4 then Color.darkMint else Color.lightGrey
           , cornerRadius 4.0
           , cornerRadii $ if key == "back" then Corners 30.0 false false false true else Corners 30.0 false false true false
           , onClick push if key == "back" then (const (OnClickBack state.text)) else (const (OnClickDone state.text))
           , clickable if key == "back" then true else if (length state.text) == 4 then true else false 
           ][ if key == "back" then 
                textView
                [ width WRAP_CONTENT
                , height MATCH_PARENT
                , text key
                , color Color.greyTextColor
                , textSize FontSize.a_21
                , fontStyle $ FontStyle.bold LanguageStyle
                , padding (Padding 0 15 0 15)
                ]
                else 
                  imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback if (length state.text) == 4 then "ny_ic_tick_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_tick_white.png" else "ny_ic_tick_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_tick_grey.png" 
                  , margin (Margin 0 18 0 18)
                  ]
           ]
           else
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , background Color.white900
           , cornerRadius 4.0
           , onClick push (const (OnSelection key state.focusIndex))
           ][  textView
               [ width WRAP_CONTENT
               , height MATCH_PARENT
               , text key
               , color Color.greyTextColor
               , textSize FontSize.a_21
               , fontStyle $ FontStyle.bold LanguageStyle
               , padding (Padding 0 15 0 15)
               ]
           ]
       ]
       ) item.keys )
    ) state.keyList )