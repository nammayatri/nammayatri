{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.View where

import Common.Types.App (LazyCheck(..))
import Components.RateCard.Controller (Action(..), Config)
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (Merchant(..), getMerchant)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>),(==), (||), (&&), map, show, not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml,imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight,fontStyle, scrollView)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Screens.Types (RateCardType(..))
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 20 20
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
     ][linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background if config.nightCharges && config.nightChargesApplicable then Color.black900 else Color.blue600'
        , orientation HORIZONTAL
        , cornerRadii $ Corners 16.0 true true false false
        ][ 
          linearLayout
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , orientation VERTICAL
           , gravity CENTER_VERTICAL
           , padding (Padding 16 22 16 0)
           , margin (MarginBottom 8)
           ]([ textView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , textSize FontSize.a_24
              , lineHeight "28"
              , color if config.nightCharges && (config.nightChargesApplicable) then Color.white900 else Color.black800
              , text config.title
              , fontStyle $ FontStyle.bold LanguageStyle
              , margin (MarginVertical 4 4)
              ]
            ] <>  if config.nightChargesApplicable then 
                    [textView
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , textSize FontSize.a_14
                    , lineHeight "15"
                    , fontStyle $ FontStyle.medium LanguageStyle
                    , color if config.nightCharges then Color.black500 else Color.black700
                    , text if config.nightCharges then (getString NIGHT_TIME_CHARGES) else (getString DAY_TIME_CHARGES)
                    
                    ]] else [])
         , imageView
           [ width MATCH_PARENT
           , height $ V 90
           , visibility if config.nightChargesApplicable then VISIBLE else GONE
           , imageWithFallback if config.nightCharges then "ny_ic_night,https://assets.juspay.in/nammayatri/images/user/ny_ic_night.png" else "ny_ic_day,https://assets.juspay.in/nammayatri/images/user/ny_ic_day.png"
           ]  
         ]
      ,linearLayout
        [ width MATCH_PARENT
        , height $ V 350
        , orientation HORIZONTAL
        ][PrestoAnim.animationSet [ if config.currentRateCardType == DefaultRateCard then (translateInXBackwardAnim config.onFirstPage) else (translateInXForwardAnim true) ] $
          if config.currentRateCardType == DefaultRateCard then defaultRateCardView push config 
          else if config.currentRateCardType == DriverAddition then driverAdditionView push config 
          else fareUpdateView push config
        ]     
      ,linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.blue800
        , gravity CENTER
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , text if config.currentRateCardType == DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
        , textSize FontSize.a_18
        , lineHeight "20"
        , padding (Padding 0 8 0 25)
        , onClick push if config.currentRateCardType == DefaultRateCard then const Close else const GoToDefaultStart
        ]
      ]
    ]      
  ]
defaultRateCardView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
defaultRateCardView push config = 
  scrollView
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][  linearLayout  
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL 
      ](map (\item -> 
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , margin (MarginVertical 10 10)
              , padding (PaddingHorizontal 20 20)
              ][  textView 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_16 
                  , lineHeight "18"
                  , fontStyle $ FontStyle.medium LanguageStyle
                  , color Color.black800
                  , text item.title 
                  ]
                , textView 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_16 
                  , color Color.black800
                  , text item.description 
                  , gravity RIGHT 
                  , weight 1.0
                  ]
                ] ) config.rateCardArray)

    , imageView
      [ width MATCH_PARENT
      , height $ V 2 
      , imageWithFallback "ny_ic_horizontal_dash,https://assets.juspay.in/nammayatri/images/user/ny_ic_horizontal_dash.png"
      , margin (Margin 20 20 20 12)
      , visibility if config.nightChargesApplicable then VISIBLE else GONE
      ]
    , textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , color Color.black700
      , text if config.nightCharges && config.nightChargesApplicable then
                    (getString NIGHT_TIMES_OF) <> config.nightShiftMultiplier <> (getString DAYTIME_CHARGES_APPLIED_AT_NIGHT)
                  else
                    (getString DAY_TIMES_OF) <> config.nightShiftMultiplier <> (getString DAYTIME_CHARGES_APPLICABLE_AT_NIGHT)
      ,  textSize FontSize.a_14
      , lineHeight "16"
      , visibility if config.nightChargesApplicable then VISIBLE else GONE
      , fontStyle $ FontStyle.regular LanguageStyle
      , padding $ PaddingHorizontal 20 20
      ]
    , imageView
      [ width MATCH_PARENT
      , height $ V 2 
      , imageWithFallback "ny_ic_horizontal_dash,https://assets.juspay.in/nammayatri/images/user/ny_ic_horizontal_dash.png"
      , margin (Margin 20 12 20 0)
      ]
    , listItem push { action : GoToDriverAddition, title : (getString DRIVER_ADDITIONS)}
    , linearLayout 
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey800
      , margin $ MarginHorizontal 16 16
      ][]
    , listItem push { action : GoToFareUpdate, title : (getString FARE_UPDATE_POLICY)}
      ]]
    
listItem :: forall w . (Action -> Effect Unit) -> {title :: String , action :: Action} -> PrestoDOM (Effect Unit) w 
listItem push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin (MarginVertical 12 12)
  , padding (Padding 20 0 20 0)
  , onClick push (const config.action)
  ][  textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black700
      , text config.title
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , padding (Padding 0 0 20 0)
      ]
    , linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , padding (Padding 20 0 0 0)
      , gravity RIGHT
      ][  imageView
          [ height $ V 12
          , width $ V 12
          , imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
          , margin $ MarginTop 4
          , color Color.black900
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ] 
        ]              
    ]

driverAdditionView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
driverAdditionView push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][  textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_18
      , lineHeight "20"
      , fontStyle $ FontStyle.semiBold LanguageStyle
      , color Color.black800
      , text (getString DRIVER_ADDITIONS_OPTIONAL)
      , padding (Padding 20 0 20 0)
      , margin $ MarginTop 20
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , text (getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC)
      , padding (Padding 20 0 20 0)
      , margin $ MarginTop 12
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , text config.driverAdditionsLogic 
      , padding (Padding 20 0 20 0)
      , margin $ MarginTop 12
      ]
    , imageView
      [ height $ V 100
      , width MATCH_PARENT
      , imageWithFallback config.driverAdditionsImage 
      , margin $ MarginTop 12
      ] 
    , textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , text (getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE)
      , padding (Padding 20 0 20 0)
      , margin $ MarginTop 12
      ]
    ]

fareUpdateView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
fareUpdateView push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][  textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_16
      , lineHeight "20"
      , fontStyle $ FontStyle.semiBold LanguageStyle
      , color Color.black800
      , text (getString FARE_UPDATE_POLICY)
      , margin $ MarginTop 20
      , padding (Padding 20 0 20 0)
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , text (getString YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS)
      , margin $ MarginTop 8
      , padding (Padding 20 0 20 0)
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , textFromHtml $ "<span style=\"color:black;\">" <> (getString REASON_CHANGE_IN_ROUTE_A) <> "</span>" <> (getString REASON_CHANGE_IN_ROUTE_B)
      , margin $ MarginTop 20
      , padding (Padding 20 0 20 180)
      ]
  ]

getAdditionalFare :: String -> Int
getAdditionalFare additionalFare = DM.fromMaybe 0 $ DI.fromString $ DS.drop 1 additionalFare

rateCardComponentView :: forall w. (Action -> Effect Unit) -> Config -> {title :: String, value :: Int} -> PrestoDOM (Effect Unit) w 
rateCardComponentView push config item = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin (MarginVertical 10 10)
  , padding (Padding 20 0 20 0)
  ][ textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_16
      , lineHeight "18"
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.black800
      , text $ item.title
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_16
      , color Color.black800
      , lineHeight "18"
      , text (show $ item.value)
      , fontStyle $ FontStyle.medium LanguageStyle
      , gravity RIGHT
      , weight 1.0
      ]
    ]