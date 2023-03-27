{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SourceToDestination.View where

import Prelude (Unit, ($), (<>))
import Effect (Effect)
import Components.SourceToDestination.Controller (Action,Config)
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Margin(..), Padding(..), background, color, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, maxLines, orientation, padding, text, textSize, textView, visibility, width, cornerRadius, stroke, margin, imageWithFallback)
import Styles.Colors as Color
import Font.Size as FontSize

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  frameLayout
  [ height WRAP_CONTENT
  , width config.width
  , gravity LEFT
  , margin config.margin
  ][  imageView
        [ imageWithFallback "ny_ic_line_img,https://assets.juspay.in/nammayatri/images/user/ny_ic_line_img.png"
        , height MATCH_PARENT
        , margin config.lineMargin
        , width (V 1)
        ]
    , distanceLayout config
    , linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL 
      , width MATCH_PARENT
      ][ sourceLayout config 
      , destinationLayout config 
      ]
    ]
    

sourceLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
sourceLayout config =
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , margin config.sourceMargin
  ][  imageView
      [ width config.sourceImageConfig.width
      , height config.sourceImageConfig.height
      , imageWithFallback config.sourceImageConfig.imageUrl
      , margin config.sourceImageConfig.margin 
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      ][  textView
          [ text config.sourceTextConfig.text
          , textSize config.sourceTextConfig.textSize
          , width MATCH_PARENT
          , padding config.sourceTextConfig.padding
          , margin config.sourceTextConfig.margin
          , fontStyle config.sourceTextConfig.fontStyle
          , color config.sourceTextConfig.color
          , ellipsize config.sourceTextConfig.ellipsize
          , maxLines config.sourceTextConfig.maxLines
          ]
        , textView
          [ text config.rideStartedAtConfig.text
          , textSize config.rideStartedAtConfig.textSize
          , fontStyle config.rideStartedAtConfig.fontStyle
          , color config.rideStartedAtConfig.color
          , visibility config.rideStartedAtConfig.visibility
          , margin config.rideStartedAtConfig.margin 
          , padding config.rideStartedAtConfig.padding
          , maxLines config.rideStartedAtConfig.maxLines
          , ellipsize config.rideStartedAtConfig.ellipsize
          ]
        ]
    
    ]

destinationLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
destinationLayout config =
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , margin config.destinationMargin
  ][  linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , margin config.destinationImageConfig.margin 
      , background config.destinationBackground
      ][  imageView
          [ width config.destinationImageConfig.width
          , height config.destinationImageConfig.height
          , imageWithFallback config.destinationImageConfig.imageUrl
          ]
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      ][  textView
          [ text config.destinationTextConfig.text
          , textSize config.destinationTextConfig.textSize
          , layoutGravity "center_vertical"
          , padding config.destinationTextConfig.padding
          , width MATCH_PARENT
          , margin config.destinationTextConfig.margin
          , fontStyle config.destinationTextConfig.fontStyle
          , color config.destinationTextConfig.color
          , maxLines config.destinationTextConfig.maxLines
          , ellipsize config.destinationTextConfig.ellipsize
          ]
        , textView
          [ text config.rideEndedAtConfig.text
          , textSize config.rideEndedAtConfig.textSize
          , fontStyle config.rideEndedAtConfig.fontStyle
          , color config.rideEndedAtConfig.color
          , visibility config.rideEndedAtConfig.visibility
          , margin config.rideEndedAtConfig.margin 
          , padding config.rideEndedAtConfig.padding
          , maxLines config.rideEndedAtConfig.maxLines
          , ellipsize config.rideEndedAtConfig.ellipsize
          ]
        ]
    ]

distanceLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
distanceLayout config = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , stroke $ "1," <> Color.grey900
  , cornerRadius 4.0
  , margin config.distanceConfig.margin
  , background config.distanceConfig.background
  , visibility config.distanceConfig.distanceVisibility
  ][ textView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , text config.distanceConfig.distanceValue
      , textSize FontSize.a_12
      , color Color.black900
      , padding $ Padding 6 4 6 4
      ]
  ]