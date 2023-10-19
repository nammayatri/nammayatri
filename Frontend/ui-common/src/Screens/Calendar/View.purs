{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Calendar.View where

import Prelude
import Calendar.Controller (Action(..), CalendarScreenState, dummyDateItem, dummyStartDateItem, dummyEndDateItem, ScreenOutput, eval)
import PrestoDOM.Animation as PrestoAnim
import Effect (Effect)
import PrestoDOM (onBackPressed, ScopedScreen, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), scrollBarY, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, id, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, editText, onChange, hint, scrollView, onAnimationEnd, pattern, ellipsize, clickable, singleLine, maxLines, hintColor, imageWithFallback, adjustViewWithKeyboard, accessibilityHint, accessibility, relativeLayout, horizontalScrollView, scrollBarX)
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
import Data.Number (min)
import Debug (spy)
import Data.Function.Uncurried (runFn2)

screen :: CalendarScreenState -> ScopedScreen Action CalendarScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "Calendar"
  , parent : Just "Calendar"
  , globalEvents : []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black9000
  , orientation VERTICAL
  , gravity CENTER
  , onClick push $ const DismissCalendarScreen
  , onBackPressed push (const BackPressed)
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , margin $ Margin 16 16 16 16
     , padding $ PaddingBottom 16
     , orientation VERTICAL
     , background Color.white900
     , gravity CENTER
     , cornerRadius 9.0
     , onClick push $ const NoAction
     ][   monthYearPicker push config
       ,  calendarView push config
       ,  separator push config
       ,  selectedDateView push config
       ,  PrimaryButton.view (push <<< PrimaryButtonActionController) (config.calendarScreenConfig.primaryButtonConfig)
       ,  PrimaryButton.view (push <<< PrimaryButtonCancelActionController) (config.calendarScreenConfig.cancelButtonConfig)
     ]
  ]

calendarView :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
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


separator :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
separator push config =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  , margin $ MarginTop 5
  ][]

selectedDateView :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
selectedDateView push config =
  let startDate = case config.calendarConfig.startDate of
                    Nothing -> dummyStartDateItem
                    Just date -> date
      endDate = case config.calendarConfig.endDate of
                    Nothing -> dummyEndDateItem
                    Just date -> date
      errorConfig = runFn2 config.calendarScreenConfig.errorHandler config.calendarConfig.startDate config.calendarConfig.endDate
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ Margin 16 16 16 16
  ][  textView
      [ text $ config.calendarScreenConfig.defaultMessage
      , color $ Color.black600
      , textSize $ FontSize.a_14
      , fontStyle $ FontStyle.regular LanguageStyle
      , visibility $ case config.calendarConfig.startDate of
                      Nothing -> VISIBLE
                      Just _ -> GONE
      ]
    , textView
      [ text $ errorConfig.errorMessage
      , color $ Color.red
      , textSize $ FontSize.a_14
      , fontStyle $ FontStyle.regular LanguageStyle
      , visibility $ if errorConfig.showError then VISIBLE else GONE
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , visibility $ case config.calendarConfig.startDate of
                      Nothing -> GONE
                      Just _ -> if errorConfig.showError then GONE else VISIBLE
      ][  textView
          [ text $ startDate.shortMonth <> " " <> (if startDate.date < 10 then "0"  else "") <> show startDate.date <> ", " <> show startDate.year
          , color Color.black800
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , gravity CENTER
          ]
        , imageView
          [ imageWithFallback $ config.imageConfig.rightArrowImage
          , height MATCH_PARENT
          , width $ V 16
          , margin $ MarginHorizontal 8 8
          , visibility $ case config.calendarConfig.endDate of
                          Nothing -> GONE
                          Just _ -> VISIBLE
          , gravity CENTER
          ]
        , textView
          [ text $ endDate.shortMonth <> " " <> (if endDate.date < 10 then "0"  else "") <> show endDate.date <> ", " <> show endDate.year
          , color Color.black800
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , visibility $ case config.calendarConfig.endDate of
                          Nothing -> GONE
                          Just _ -> VISIBLE
          , gravity CENTER
          ]
      ]
  ]



monthYearPicker :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
monthYearPicker push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , background Color.white900
  , margin $ Margin 16 16 16 16
  , cornerRadius 9.0
  ][  imageView
      [ imageWithFallback $ if decrementMonthFlag then config.imageConfig.chevronLeftImageBlack else config.imageConfig.chevronLeftImageGrey
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
          [ text $ config.calendarConfig.selectedTimeSpan.shortMonth
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.black800
          ]
        , textView
          [ text $ show config.calendarConfig.selectedTimeSpan.year
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.black800
          , margin $ MarginLeft 5
          ]
      ]
    , imageView
      [ imageWithFallback $ if incrementMonthFlag then config.imageConfig.chevronRightImageBlack else config.imageConfig.chevronRightImageGrey
      , width $ V 28
      , height $ V 28
      , gravity CENTER
      , onClick push $ const if incrementMonthFlag then IncrementMonth incrementRes else NoAction
      ]
  ]

  where incrementMonthFlag = (config.calendarConfig.selectedTimeSpan.intMonth < config.calendarScreenConfig.futureLimit.intMonth && config.calendarConfig.selectedTimeSpan.year == config.calendarScreenConfig.futureLimit.year) ||  (config.calendarConfig.selectedTimeSpan.year < config.calendarScreenConfig.futureLimit.year)
        incrementRes = EHC.incrementCalendarMonth config.calendarConfig.selectedTimeSpan config.calendarConfig.startDate config.calendarConfig.endDate
        decrementMonthFlag = (config.calendarConfig.selectedTimeSpan.intMonth > config.calendarScreenConfig.pastLimit.intMonth && config.calendarConfig.selectedTimeSpan.year == config.calendarScreenConfig.pastLimit.year) || (config.calendarConfig.selectedTimeSpan.year > config.calendarScreenConfig.pastLimit.year)
        decrementRes = EHC.decrementCalendarMonth config.calendarConfig.selectedTimeSpan config.calendarConfig.startDate config.calendarConfig.endDate

datePicker :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
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

weekView :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
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

datesRowView :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> PrestoDOM (Effect Unit) w
datesRowView push config =
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
       
      ]) config.calendarConfig.weeks)

dateView :: forall w. (Action -> Effect Unit) -> CalendarScreenState -> Array CalendarModalDateObject -> PrestoDOM (Effect Unit) w
dateView push config weekRow =
  let startDate = case config.calendarConfig.startDate of
                    Nothing -> dummyStartDateItem
                    Just date -> date
      endDate = case config.calendarConfig.endDate of
                    Nothing -> dummyEndDateItem
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
            , background $ if ( item.day == startDate.day  || item.day == endDate.day ) && item.date /= 0 then Color.blue9000 
                             else if item.isInRange then Color.blue9000
                             else Color.transparent
            , onClick push $ const $ if (selectDateFlag item) then SelectDate (getSelectedDateRes item) else NoAction
            ][ textView
               [ width $ MATCH_PARENT
               , height $ MATCH_PARENT
               , text $ if item.date > 0 then show item.date else ""
               , color $ if item.date == 0 || (item.day < config.calendarScreenConfig.pastLimit.day || item.day > config.calendarScreenConfig.futureLimit.day) then Color.black500
                         else if highlightSelectedDate item startDate endDate then Color.white900
                         else Color.black800
               , textSize FontSize.a_14
               , fontStyle $ FontStyle.regular LanguageStyle
               , background $  if highlightSelectedDate item startDate endDate then Color.blue800
                               else Color.transparent
               , gravity CENTER
               , cornerRadii $ if highlightSelectedDate item startDate endDate then Corners 90.0 true true true true
                               else Corners 90.0 false false false false
               ]
            ]) weekRow)

  where selectDateFlag item = (item.day >= config.calendarScreenConfig.pastLimit.day) && ( item.day <= config.calendarScreenConfig.futureLimit.day)
        getSelectedDateRes item = if config.calendarScreenConfig.selectRange then EHC.selectRangeCalendarDate item config.calendarConfig.startDate config.calendarConfig.endDate config.calendarConfig.weeks
                                  else EHC.selectSingleCalendarDate item config.calendarConfig.startDate config.calendarConfig.endDate config.calendarConfig.weeks
        highlightSelectedDate item startDate endDate = item.date /= 0 && (item.day == startDate.day || item.day == endDate.day)