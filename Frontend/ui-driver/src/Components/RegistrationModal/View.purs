{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RegistrationModal.View where


import Components.RegistrationModal.Controller
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Common.Types.App

view :: forall w .  (Action  -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
  [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
    , gravity BOTTOM
    , onClick push (const OnCloseClick)
  ][
   PrestoAnim.animationSet [
      translateYAnim translateYAnimConfig
    ] $ 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadius 14.0
      , orientation VERTICAL
      , background Color.greyBG
      , padding (Padding 20 20 20 20)
      , clickable true
      ][ 
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation HORIZONTAL  
          ][
            linearLayout
            [ orientation VERTICAL
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , weight 1.0
            ][ textView 
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , fontStyle $ FontStyle.bold LanguageStyle
                , color Color.textPrimary
                , textSize FontSize.a_26
                , margin (Margin 0 20 0 10)
                , text (getString REGISTRATION_STEPS)-- $ state.txtMessage  
                ]
             , textView
                [ text (getString PROGRESS_SAVED)
                , textSize FontSize.a_16
                , margin (MarginBottom 20)
                ]
            ]
            , imageView
            [ width (V 20)
            , height (V 20)
            , margin (MarginTop 23)
            , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
            , onClick push (const OnCloseClick)
            ]
          ]
        ,   linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ][
            checkBar state
        ,   cardItemView state
        ]
      ]
  ]

checkBar :: State -> forall w . PrestoDOM (Effect Unit) w
checkBar state = 
    linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin (Margin 0 20 15 10)
    ][  imageView
        [ imageWithFallback "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
        , width (V 27)
        , height (V 27)
        ]
    ,   linearLayout
        [ height (V 75)
        , width (V 1)
        , background Color.grey900
        , margin (MarginLeft 13)
        ][]
    ,   imageView
        [ imageWithFallback "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
        , width (V 27)
        , height (V 27)
        ]
    ,   linearLayout
        [ height (V 75)
        , width (V 1)
        , background Color.grey900
        , margin (MarginLeft 13)
        ][]
    ,   imageView
        [ imageWithFallback "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
        , width (V 27)
        , height (V 27)
        ]
    ,   linearLayout
        [ height (V 75)
        , width (V 1)
        , background Color.grey900
        , margin (MarginLeft 13)
        ][]
    ,   imageView
        [ imageWithFallback "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
        , width (V 27)
        , height (V 27)
        ]
    ]

cardItemView :: State -> forall w . PrestoDOM (Effect Unit) w
cardItemView state = 
    linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ](map
        (\item -> 
            linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , stroke ("1," <> Color.grey900)
    , margin (MarginBottom 24)
    , padding (Padding 15 17 35 17)
    , cornerRadius 3.0
    ][ imageView
        [ imageWithFallback item.img
        , width (V 40)
        , height (V 40)
        , margin (MarginRight 14)
        ]
    ,   linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding (PaddingRight 10)
        ][ textView
            [ text item.mainTxt
            , fontStyle $ FontStyle.bold LanguageStyle
            , textSize FontSize.a_16
            , color Color.black900
            ]
            , textView
            [ text item.subTxt
            , textSize FontSize.a_16
            ]
        ]
    ]
        )
        (cardArr state)
    )
cardArr :: State ->  Array { img :: String , mainTxt :: String, subTxt :: String }
cardArr state = [
          { mainTxt : (getString DRIVING_LICENSE),
            subTxt : (getString UPLOAD_FRONT_BACK),
            img : "ny_ic_license_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_license_blue.png"
          },
          { mainTxt : (getString AADHAR_CARD),
            subTxt : (getString UPLOAD_FRONT_BACK),
            img : "ny_ic_aadhaar,https://assets.juspay.in/nammayatri/images/driver/ny_ic_aadhaar.png"
          },
          { mainTxt : (getString BANK_DETAILS),
            subTxt : (getString EARNINGS_WILL_BE_CREDITED),
            img : "ny_ic_bank_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_bank_blue.png"
          },
          { mainTxt : (getString VEHICLE_DETAILS),
            subTxt : (getString FILL_VEHICLE_DETAILS),
            img : "ny_ic_car_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_car_blue.png"
          }
          ]