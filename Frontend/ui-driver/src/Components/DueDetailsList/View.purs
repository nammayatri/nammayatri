{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DueDetailsList.View where

import Common.Types.App
import Components.DueDetailsList.Controller

import Data.Array (length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Maybe as Mb
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getFixedTwoDecimals)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, show, unit, void, ($), (&&), (/=), (<>), (==), (||), (<))
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import Screens.Types (PromoConfig)
import Services.API (FeeType(..))
import Styles.Colors as Color


view :: forall w . (Action -> Effect Unit) -> DueDetailsListState -> PrestoDOM (Effect Unit) w
view push state =
    scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ linearLayout
       [ width MATCH_PARENT
       , height MATCH_PARENT
       , orientation VERTICAL
       , margin (Margin 16 10 16 10)
       ](mapWithIndex
         (\index item ->
          let rideNumberPrefix = if item.noOfRides < 10 then "0" else ""
          in
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , weight 1.0
          , orientation VERTICAL
          , padding $ Padding 16 20 16 8
          , gravity CENTER
          , stroke if item.expanded then ("1,"<>Color.blue800) else ("1,"<>Color.grey900)
          , onClick push $ const $ SelectDue item
          , margin $ MarginBottom 16
          , cornerRadius 10.0
          , background if item.expanded then Color.blue600 else Color.white900
          ][ 
            linearLayout[
              width MATCH_PARENT
              , height WRAP_CONTENT
              , margin $ MarginBottom 16
            ]
            [
              linearLayout[
                orientation VERTICAL
                , weight 1.0
              ][
                linearLayout[][
                  textView $ [
                    text (getString TRIP_DATE)
                    , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
                  , textView $ [
                    text item.date
                    , color Color.black800
                    , margin $ MarginLeft 2
                  ] <> FontStyle.body6 TypoGraphy
                ]
                , textView [
                    text item.planType
                    , textSize FontSize.a_14
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color Color.black700
                  ]
              ]
              , linearLayout[
                height MATCH_PARENT
                , gravity CENTER_VERTICAL
              ][
                textView $ [
                  text $ "₹" <> getFixedTwoDecimals item.dueAmount <> if item.isAutoPayFailed || item.isSplitPayment then "*" else ""
                  , color Color.black800
                ] <> FontStyle.h2 TypoGraphy
                , imageView
                  [ imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item.expanded then "ny_ic_chevron_up" else "ny_ic_chevron_down"
                  , height (V 11)
                  , margin (Margin 5 2 0 0)
                  , width (V 11)
                  ]
              ]
            ]
            , linearLayout [
              orientation VERTICAL
              , width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility if item.expanded then VISIBLE else GONE
            ][
                linearLayout [
                  height $ V 1
                  , width MATCH_PARENT
                  , background Color.grey900
                ][]
                , keyValueView (getString NUMBER_OF_RIDES) (rideNumberPrefix <> show item.noOfRides) true false FontStyle.Body3 16 Color.black700
                , keyValueView (getString PAYMENT_MODE) (if item.paymentMode == AUTOPAY_PAYMENT then getString UPI_AUTOPAY_S else "UPI") true true FontStyle.Body3 16 Color.black700
                , keyValueView (getString SCHEDULED_AT) (fromMaybe "" item.scheduledAt) (isJust item.scheduledAt) false FontStyle.Body3 16 Color.black700
                , keyValueView (getString PAYMENT_STATUS) (fromMaybe "" item.paymentStatus) (isJust item.paymentStatus) false FontStyle.Body3 16 Color.black700
                , keyValueView (getString YOUR_EARNINGS) ("₹" <> getFixedTwoDecimals item.totalEarningsOfDay) true false FontStyle.Body3 16 Color.black700
                , keyValueView (getString FARE_BREAKUP) (item.fareBreakup <>" "<> getString GST_INCLUDE) true false FontStyle.Body3 16 Color.black700
                , maybe (linearLayout[visibility GONE][]) (\boothCharges -> keyValueView (getString BOOTH_CHARGES) boothCharges true false FontStyle.Captions 6 Color.black600) $ item.boothCharges
                , linearLayout [
                  height WRAP_CONTENT
                  , width MATCH_PARENT
                  , margin $ MarginVertical 16 16
                  , gravity CENTER_VERTICAL
                ][
                  textView $ [
                    text $ getString OFFER_APPLIED
                    , margin $ MarginRight 8
                    , color Color.black700
                  ] <> FontStyle.body3 TypoGraphy
                  , case item.offerApplied of
                      Just offerConfig -> case fromMaybe "" offerConfig.title of 
                                            "" -> textView $
                                                  [ text "N/A"
                                                  , color Color.black900
                                                  ] <> FontStyle.body6 TypoGraphy
                                            _ -> promoCodeView push offerConfig
                      Nothing -> linearLayout[visibility GONE][]
                ]
                , textView $ [
                    text $ getString SWITCHED_TO_MANUAL
                    , width MATCH_PARENT
                    , margin $ MarginRight 8
                    , color Color.black600
                    , visibility if item.isAutoPayFailed then VISIBLE else GONE
                    , padding $ Padding 8 8 8 8
                    , background Color.white900
                    , cornerRadius 4.0
                  ] <> FontStyle.tags TypoGraphy
                , linearLayout
                  [
                    height $ V 1
                    , width MATCH_PARENT
                    , background Color.grey900
                    , margin $ MarginHorizontal 8 16
                    , visibility if item.isAutoPayFailed && item.isSplitPayment then VISIBLE else GONE
                  ][]
                , textView $ [
                    text $ getString SPLIT_PAYMENT
                    , width MATCH_PARENT
                    , margin $ MarginRight 8
                    , color Color.black600
                    , visibility if item.isSplitPayment then VISIBLE else GONE
                    , padding $ Padding 8 8 8 8
                    , cornerRadius 4.0
                    , background Color.white900
                  ] <> FontStyle.tags TypoGraphy
            ]
           ]
         ) state.dues
         )
    ]

keyValueView :: forall w properties . String -> String -> Boolean -> Boolean -> FontStyle.Style -> Int -> String -> PrestoDOM (Effect Unit) w
keyValueView key value visibility' prefixImage keyFont marginTop keyColor = 
  linearLayout 
  [
    height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop marginTop
    , visibility if visibility' then VISIBLE else GONE
    , gravity CENTER_VERTICAL
  ][ 
    textView $ [
      text key
      , margin $ MarginRight marginTop
      , color keyColor
    ] <> FontStyle.getFontStyle keyFont TypoGraphy
    , imageView
    [ width $ V 12
    , height $ V 12
    , margin (Margin 0 1 4 0)
    , visibility if prefixImage then VISIBLE else GONE
    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_upi_logo"
    ]
    , textView $ [
      text value
      , color Color.black800
    ] <> FontStyle.body6 TypoGraphy
  ]

promoCodeView :: forall w. (Action -> Effect Unit) -> PromoConfig -> PrestoDOM (Effect Unit) w 
promoCodeView push state =
  linearLayout
  ([ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  ]<> if state.isGradient then [gradient (Linear 90.0 state.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if state.hasImage then VISIBLE else GONE
     , imageWithFallback state.imageURL
     ] 
   , textView $
     [ textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ] <> case state.title of
          Mb.Nothing -> [visibility GONE]
          Mb.Just txt -> [text txt]
  ]