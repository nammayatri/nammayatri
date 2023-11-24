{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RideAllocationModal.View where

import Prelude (Unit, bind, const, discard, pure, unit, ($), (*), (+), (-), (/), (<>), (>), (>=), show)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, Padding(..), afterRender, frameLayout, imageView, linearLayout, textView, onClick, clickable, maxLines, alpha, relativeLayout, imageWithFallback)
import Components.RideAllocationModal.Controller (Action(..), Config)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import PrestoDOM.Properties (background, color, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, stroke, text, textSize, width, padding, ellipsize, alignParentLeft, alignParentRight)
import Engineering.Helpers.Commons (screenWidth, screenHeight, flowRunner)
import Language.Strings (getString)
import Language.Types (STR(..))
import Helpers.Utils (countDown, toStringJSON, parseFloat)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Except.Trans (runExceptT)
import Common.Types.App
import ConfigProvider
import Constants

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
    frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , afterRender (\action -> do
                          _ <- push action
                          countDown config.seconds config.id push CountDown
                          pure unit
                        ) (const NoAction)
    ][ linearLayout
       [ width config.width
       , height config.height
       , cornerRadius 10.0
       , orientation VERTICAL
       , margin config.margin
       , background config.background
       ][  linearLayout
           [ width MATCH_PARENT
           , height (V 1)
           , margin (Margin 20 20 20 0)
           ][]
         , frameLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , padding (PaddingHorizontal ((screenWidth unit)/18) 20)
           , orientation VERTICAL
           ][  source config
             , imageView
               [ width (V 2)
               , height MATCH_PARENT
               , imageUrl "dashed_line"
               , margin (Margin 4 38 0 40)
               ]
             , destinationArea config
             , destination config
           ]
         , distanceView config
         , linearLayout
           [ width MATCH_PARENT
           , height (V 1)
           , background Color.lightGrey
           , margin (Margin 20 10 20 10)
           ][]
         , linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , padding (Padding ((screenWidth unit)/18) 0 ((screenWidth unit)/18) 0)
           , orientation VERTICAL
           ][  linearLayout
               [ width MATCH_PARENT
               , height WRAP_CONTENT
               , orientation HORIZONTAL
               , gravity CENTER
               , background Color.grey700
               , cornerRadius 8.0
               ][  reducePrice push config
                 , totalPrice config
                 , increasePrice push config
               ]
           ]
         , linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , padding (Padding 0 ((screenHeight unit)/32) 0 ((screenHeight unit)/50))
           , gravity CENTER
           , orientation HORIZONTAL
           ][  declineButton push config
             , requestButton push config
           ]
       ]
      , countDownView config
    ]

countDownView :: forall w . Config -> PrestoDOM (Effect Unit) w
countDownView config =
  textView
  [ width (V 60)
  , height (V 60)
  , cornerRadius 30.0
  , margin (Margin ((screenWidth unit)/20 + ((screenWidth unit)/5 * 2) - 30) ((screenHeight unit)/5 - 30) ((screenWidth unit)/20) 0)
  , background Color.white900
  , stroke ("1," <> Color.grayDarker)
  , text config.countDown.text
  , gravity CENTER
  , textSize config.countDown.textSize
  , color config.countDown.textColor
  ]

source :: forall w . Config -> PrestoDOM (Effect Unit) w
source config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding (PaddingTop 20)
  , gravity TOP_VERTICAL
  ][ imageView
     [ width config.source.imageWidth
     , height config.source.imageHeight
     , imageWithFallback config.source.imageUrl
     , margin (MarginTop 5)
     ]
   , textView
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , margin (MarginLeft 10)
     , text config.source.text
     , textSize config.source.textSize
     , color config.source.textColor
     , maxLines 3
     ]
  ]

destinationArea :: forall w . Config -> PrestoDOM (Effect Unit) w
destinationArea config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding (PaddingTop 80)
  , gravity CENTER_VERTICAL
  ][ textView
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , margin (MarginLeft 20)
     , text config.destinationArea.text
     , textSize config.destinationArea.textSize
     , color config.destinationArea.textColor
     , fontStyle config.destinationArea.fontStyle
     , maxLines 1
     , ellipsize true
     ]
  ]

destination :: forall w . Config -> PrestoDOM (Effect Unit) w
destination config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding (PaddingTop 110)
  , gravity TOP_VERTICAL
  ][ imageView
     [ width config.destination.imageWidth
     , height config.destination.imageHeight
     , imageWithFallback config.destination.imageUrl
     , margin (MarginTop 5)
     ]
   , textView
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , margin (MarginLeft 10)
     , text config.destination.text
     , textSize config.destination.textSize
     , color config.destination.textColor
     , fontStyle config.destination.fontStyle
     , maxLines 3
     ]
  ]

distanceView :: forall w. Config -> PrestoDOM (Effect Unit) w
distanceView config =
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    ][ linearLayout
       [ width WRAP_CONTENT
       , height WRAP_CONTENT
       , orientation VERTICAL
       , padding (Padding 40 20 0 10)
       , gravity LEFT
       , alignParentLeft "true,-1"
       ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , text (getString PICKUP)
           , color Color.black700
           ] <> FontStyle.body14 LanguageStyle
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , text (show config.pickupDistance <> ".0 km")
           , color Color.greyTextColor
           ] <> FontStyle.h3 LanguageStyle
       ]
     , linearLayout
       [ width WRAP_CONTENT
       , height WRAP_CONTENT
       , orientation VERTICAL
       , padding (Padding 0 20 40 10)
       , gravity RIGHT
       , alignParentRight "true,-1"
       ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , gravity LEFT
          ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , text (getString TRIP)
           , color Color.black700
           ] <> FontStyle.body14 LanguageStyle
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , text (show config.journeyDistance <> ".0 km")
           , color Color.greyTextColor
           ] <> FontStyle.h2 LanguageStyle
          ]
       ]
      ]

reducePrice :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
reducePrice push config =
  linearLayout
  [ width ( V ((screenWidth unit)/7))
  , height (V ((screenHeight unit)/20))
  , cornerRadius 10.0
  , margin (Margin 4 4 4 4)
  , gravity CENTER
  , clickable if (config.totalPrice > config.basePrice) then true else false
  , onClick push (const (DecreasePrice config.id))
  , background Color.white900
  , alpha if (config.totalPrice > config.basePrice) then 1.0 else 0.5
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text ("- " <> toStringJSON(config.reducePrice))
      , color if (config.totalPrice > config.basePrice) then Color.greyTextColor else Color.borderGreyColor
      ] <> FontStyle.h3 LanguageStyle
  ]

totalPrice :: forall w . Config -> PrestoDOM (Effect Unit) w
totalPrice config =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , color Color.greyTextColor
  , text ((getCurrency appConfig) <> " " <>  (parseFloat config.totalPrice 2))
  , margin (Margin 20 0 20 0)
  ] <> FontStyle.body8 LanguageStyle

increasePrice :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
increasePrice push config =
  linearLayout
  [ width ( V ((screenWidth unit)/7))
  , height (V ((screenHeight unit)/20))
  , cornerRadius 10.0
  , margin (Margin 4 4 4 4)
  , gravity CENTER
  , clickable if (config.totalPrice >= config.basePrice + config.increasePrice * 3.0) then false else true
  , onClick push (const (IncreasePrice config.id))
  , background Color.white900
  , alpha if (config.totalPrice >= config.basePrice + config.increasePrice * 3.0) then 0.5 else 1.0
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text ("+ " <> toStringJSON(config.increasePrice))
      , color if (config.totalPrice >= config.basePrice + config.increasePrice * 3.0) then Color.borderGreyColor else Color.greyTextColor
      ] <> FontStyle.h3 LanguageStyle
  ]

declineButton :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
declineButton push config =
  linearLayout
  [ width config.decline.width
  , height config.decline.height
  , cornerRadius config.decline.cornerRadius
  , stroke ("1," <> Color.black700)
  , margin (MarginRight 10)
  , gravity CENTER
  , onClick push (const (Decline config.id))
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString DECLINE)
      , color config.decline.color
      ] <> FontStyle.h2 LanguageStyle
  ]

requestButton :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
requestButton push config =
  linearLayout
  [ width config.request.width
  , height config.request.height
  , cornerRadius config.request.cornerRadius
  , background config.request.background
  , gravity CENTER
  , onClick push (const (Request config.id (config.totalPrice - config.basePrice)))
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString REQUEST)
      , color config.request.color
      ] <> FontStyle.h2 LanguageStyle
  ]
