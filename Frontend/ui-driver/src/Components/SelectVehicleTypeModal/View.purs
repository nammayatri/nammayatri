{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SelectVehicleTypeModal.View where

import Prelude (Unit, const, map, ($))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Components.SelectVehicleTypeModal.Controller (Action(..), SelectVehicleTypeModalState)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Screens.Types (VehicalTypes(..))
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))

view :: forall w . (Action  -> Effect Unit) -> SelectVehicleTypeModalState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , background Color.black9000
  , gravity BOTTOM
  , onClick push (const OnCloseClick)
  ][  PrestoAnim.animationSet 
      [ translateYAnim translateYAnimConfig] $ 
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , background Color.greyBG
        , padding (Padding 10 15 10 15)
        , clickable true
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            ][ textView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , weight 1.0
                , fontStyle $ FontStyle.medium LanguageStyle
                , color Color.textPrimary
                , textSize FontSize.a_26
                , text state.title
                , margin (Margin 10 0 0 5)
                ]
              , imageView
                [ width (V 15)
                , height MATCH_PARENT
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                , onClick push (const OnCloseClick)
                ]
            ]
          , scrollView
            [ width MATCH_PARENT
            , height (V 210)
            ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , cornerRadius 4.0
                ] (map
                    (\item -> 
                    linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , onClick push (const (OnSelect item))
                    , padding (PaddingLeft 8)
                    , gravity CENTER_VERTICAL
                    ][ imageSection item
                     , textList item
                     ]
                    )state.listItems
                  )
            ]
        ]
  ]

imageSection :: forall w . VehicalTypes -> PrestoDOM (Effect Unit) w
imageSection item = 
 linearLayout
  [ width ( V 40 )
  , height ( V 40 )
  , background Color.lightShadeGrey
  , gravity CENTER
  , cornerRadius 20.0
  ][ imageView
      [ width (V 20)
      , height (V 20)
      , imageWithFallback $ fetchImage FF_ASSET $ case item of
          Sedan     -> "ic_sedan_vehicle"
          SUV       -> "ic_suv"
          Hatchback -> "ic_hatchback"
          Auto      -> "ic_auto"
          Bike      -> "ny_ic_bike_side"
          Ambulance_Taxi -> "ic_bike_ambulance"
          Ambulance_AC -> "ic_bike_ambulance"
          Ambulance_AC_Oxy -> "ic_bike_ambulance"
          Ambulance_Taxi_Oxy -> "ic_bike_ambulance"
          Ambulance_Ventilator -> "ic_bike_ambulance"
          Suv_Plus  -> "ny_ic_suv_plus_side"
          EV_Auto   -> "ic_auto"
          HERITAGE_CAB -> "ny_ic_heritage_cab_side" 
      ]
  ]

textList :: forall w . VehicalTypes -> PrestoDOM (Effect Unit) w
textList item = 
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , orientation VERTICAL
  , margin (MarginLeft 12)
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 0 16 0 16)
      ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , color Color.greyTextColor
          , alpha 0.5
          , text $ case item of
              Sedan                 -> "Sedan"
              SUV                   -> "SUV"
              Hatchback             -> "Hatchback"
              Auto                  -> "Auto"
              Bike                  -> "Bike"
              Ambulance_Taxi        -> "Ambulance_Taxi"
              Ambulance_AC          -> "Ambulance_AC"
              Ambulance_AC_Oxy      -> "Ambulance_AC_Oxy"
              Ambulance_Taxi_Oxy    -> "Ambulance_Taxi_Oxy"
              Ambulance_Ventilator  -> "Ambulance_Ventilator"
              Suv_Plus              -> "SUV_PLUS"
              EV_Auto               -> "EV Auto"
              HERITAGE_CAB          -> "Heritage Cab"
          ] <> FontStyle.paragraphText TypoGraphy
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height (V 1)
      , background Color.borderColorLight
      ][]
  ]