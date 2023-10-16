{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Calendar.View where

import Helpers.Utils (getAssetStoreLink)

import Prelude
import Components.Calendar.Controller (Action(..), Config, dummyDateItem)
import PrestoDOM.Animation as PrestoAnim
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), scrollBarY, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, id, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, editText, onChange, hint, scrollView, onAnimationEnd, pattern, ellipsize, clickable, singleLine, maxLines, hintColor, imageWithFallback, adjustViewWithKeyboard, accessibilityHint, accessibility, relativeLayout, horizontalScrollView, scrollBarX)
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Font.Style (Style(..))
import Data.Array as DA
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Common.Types.App
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Data.Number (min)
import Debug

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black9000
  , orientation VERTICAL
  , gravity CENTER
  , onClick push $ const HideCalendarPopup
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , margin $ Margin 16 16 16 16
     , orientation VERTICAL
     , background Color.white900
     , gravity CENTER
     , cornerRadius 9.0
     , onClick push $ const NoAction
     ][   monthYearPicker push config
       ,  calendarView push config
       ,  separator push config
       ,  selectedDateView push config
       ,  PrimaryButton.view (push <<< PrimaryButtonActionController) (config.primaryButtonConfig)
       ,  PrimaryButton.view (push <<< PrimaryButtonCancelActionController) (config.cancelButtonConfig)
     ]
  ]

calendarView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
calendarView push config = 
  horizontalScrollView 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , scrollBarX false
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , gravity CENTER
     ][ datePicker push config
    ]
  ]


separator :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
separator push config =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  , margin $ MarginTop 5
  ][]

selectedDateView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
selectedDateView push config =
  let startDate = case config.startDate of
                    Nothing -> dummyDateItem
                    Just date -> date
      endDate = case config.endDate of
                    Nothing -> dummyDateItem
                    Just date -> date
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ Margin 16 16 16 16
  ][  textView
      [ text $ config.defaultMessage
      , color $ Color.black600
      , textSize $ FontSize.a_14
      , fontStyle $ FontStyle.regular LanguageStyle
      , visibility $ case config.startDate of
                      Nothing -> VISIBLE
                      Just _ -> GONE
      ]
    , textView
      [ text $ config.errorMessage
      , color $ Color.red
      , textSize $ FontSize.a_14
      , fontStyle $ FontStyle.regular LanguageStyle
      , visibility $ if config.showError then VISIBLE else GONE
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , visibility $ case config.startDate of
                      Nothing -> GONE
                      Just _ -> if config.showError then GONE else VISIBLE
      ][  textView
          [ text $ startDate.shortMonth <> " " <> (if startDate.date < 10 then "0"  else "") <> show startDate.date <> ", " <> show startDate.year
          , color Color.black800
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , gravity CENTER
          ]
        , imageView
          [ imageWithFallback $ "ny_ic_arrow_right," <> (getAssetStoreLink FunctionCall) <> "ny_ic_arrow_right.png"
          , height MATCH_PARENT
          , width $ V 16
          , margin $ MarginHorizontal 8 8
          , visibility $ case config.endDate of
                          Nothing -> GONE
                          Just _ -> VISIBLE
          , gravity CENTER
          ]
        , textView
          [ text $ endDate.shortMonth <> " " <> (if endDate.date < 10 then "0"  else "") <> show endDate.date <> ", " <> show endDate.year
          , color Color.black800
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , visibility $ case config.endDate of
                          Nothing -> GONE
                          Just _ -> VISIBLE
          , gravity CENTER
          ]
      ]
  ]



monthYearPicker :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
monthYearPicker push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , background Color.white900
  , margin $ Margin 16 16 16 16
  , cornerRadius 9.0
  ][  imageView
      [ imageWithFallback $ if decrementMonthFlag then  "ny_ic_chevron_left_black," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left_black.png"
                            else "ny_ic_chevron_left_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left_grey.png"
      , width $ V 24
      , height $ V 24
      , gravity CENTER
      , onClick push $ const if decrementMonthFlag then DecrementMonth decrementRes else NoAction
      ]
    , linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][  textView
          [ text $ config.selectedTimeSpan.shortMonth
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.black800
          ]
        , textView
          [ text $ show config.selectedTimeSpan.year
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.black800
          , margin $ MarginLeft 5
          ]
      ]
    , imageView
      [ imageWithFallback $ if incrementMonthFlag then "ny_ic_chevron_right_black," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_black.png"
                            else  "ny_ic_chevron_right_grey_900," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey_900.png"
      , width $ V 28
      , height $ V 28
      , gravity CENTER
      , onClick push $ const if incrementMonthFlag then IncrementMonth incrementRes else NoAction
      ]
  ]

  where incrementMonthFlag = config.selectedTimeSpan.intMonth < config.futureLimit.intMonth &&  config.selectedTimeSpan.year <= config.futureLimit.year
        incrementRes = EHU.incrementCalendarMonth config.selectedTimeSpan config.startDate config.endDate
        decrementMonthFlag = config.selectedTimeSpan.intMonth > config.pastLimit.intMonth &&  config.selectedTimeSpan.year >= config.pastLimit.year
        decrementRes = EHU.decrementCalendarMonth config.selectedTimeSpan config.startDate config.endDate



datePicker :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
datePicker push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 17 17
  , gravity CENTER
  ][ weekView push config
   , datesRowView push config
  ]

weekView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
weekView push config =
  let squareSize = (EHC.screenWidth unit - 32)/7
  in
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 5
  ](DA.mapWithIndex (\index item -> 
                          linearLayout
                          [ width $ V if squareSize < 42 then squareSize else 42 
                          , gravity CENTER
                          , padding $ Padding 0 5 0 5
                          ][ textView
                             [ text item
                             , color Color.black800
                             , textSize FontSize.a_14
                             , fontStyle $ FontStyle.regular LanguageStyle
                             ]
                          ]) ["S", "M", "T", "W", "T", "F", "S"])



datesRowView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
datesRowView push config =
  let _ = spy "debug cal config" config
  in
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  ](DA.mapWithIndex (\index weekRow -> 
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation HORIZONTAL
      ][ dateView push config weekRow.week
       
      ]) config.weeks)

dateView :: forall w. (Action -> Effect Unit) -> Config -> Array CalendarModalDateObject -> PrestoDOM (Effect Unit) w
dateView push config weekRow =
  let startDate = case config.startDate of
                    Nothing -> dummyDateItem
                    Just date -> date
      endDate = case config.endDate of
                    Nothing -> dummyDateItem
                    Just date -> date
      squareSize = (EHC.screenWidth unit - 32)/7
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginVertical 2 2
  ] (DA.mapWithIndex (\index item ->
         linearLayout
            [ width $ V if squareSize < 42 then squareSize else 42
            , height $ V if squareSize < 42 then squareSize else 42
            , gravity CENTER
            , cornerRadii $ if item.isEnd then Corners 50.0 false true true false
                            else if item.isStart && endDate.date /= 0 then Corners 50.0 true false false true
                            else if item.isStart && endDate.date == 0 then Corners 90.0 true true true true
                            else Corners 90.0 false false false false
            , background $ if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.blue9000 
                             else if item.isInRange then Color.blue9000
                             else Color.transparent
            , onClick push $ const $ if (selectDateFlag item) then SelectDate (getSelectedDateRes item) else NoAction
            ][ textView
               [ width $ MATCH_PARENT
               , height $ MATCH_PARENT
               , text $ if item.date > 0 then show item.date else ""
               , color $ if (item.utcDate < config.pastLimit.utcDate || item.utcDate > config.futureLimit.utcDate) then Color.black500
                         else if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.white900
                         else Color.black800
               , textSize FontSize.a_14
               , fontStyle $ FontStyle.regular LanguageStyle
               , background $  if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.blue800
                               else Color.transparent
               , gravity CENTER
               , cornerRadii $ if (item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate) then Corners 90.0 true true true true
                               else Corners 90.0 false false false false
               ]
            ]) weekRow)

  where selectDateFlag item = (item.utcDate >= config.pastLimit.utcDate && item.utcDate <= config.futureLimit.utcDate)
        getSelectedDateRes item = if config.selectRange then EHU.selectRangeCalendarDate item config.startDate config.endDate config.weeks
                                  else EHU.selectSingleCalendarDate item config.startDate config.endDate config.weeks
 