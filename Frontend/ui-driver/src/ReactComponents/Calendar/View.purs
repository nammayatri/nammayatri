{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.Calendar.View where

-- import Common.Types.App

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude (Unit, pure, show, unit, ($), (&&), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import React.Basic.Hooks (JSX, Component, component, useState)
import React.Basic.Hooks as React
import React.Render.CustomBase (linearLayout, textView, imageView)
import ReactComponents.Calendar.Controller (ComponentOutput(..), ComponentAction(..), Config, dummyDateItem, eval)
import Styles.Colors as Color

-- import Data.Number (min)

-- app :: (ComponentOutput -> Effect Unit) -> Component Config
-- app push = do
--   component "calendar" \initialState -> React.do
--     state /\ updateState <- useState initialState
--     pure $ view push state updateState

-- view :: (ComponentOutput -> Effect Unit) -> Config -> ((Config -> Config) -> Effect Unit) -> JSX
-- view push state updateState =
--   linearLayout
--     { width: "match_parent"
--     , height: "match_parent"
--     , background: Color.black9000
--     , orientation: "vertical"
--     , gravity: "center"
--     , onClick: push HideCalendarPopup
--     }
--     [ linearLayout
--         { width: "match_parent"
--         , height: "wrap_content"
--         , margin: "16, 16, 16, 16"
--         , orientation: "vertical"
--         , background: Color.white900
--         , gravity: "center"
--         , cornerRadius: "9.0"
--         , onClick: eval NoAction updateState state
--         }
--         [ monthYearPicker push state
--         , calendarView push state
--         , separator push state
--         , selectedDateView push state
--         --  , PrimaryButtonView.view (push <<< PrimaryButtonActionController) (config.primaryButtonConfig)
--         --  , PrimaryButton.view (push <<< PrimaryButtonCancelActionController) (config.cancelButtonConfig)
--         ]
--     ]

-- monthYearPicker :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- monthYearPicker _ state =
--   linearLayout
--     { width: "match_parent"
--     , height: "wrap_content"
--     , orientation: "horizontal"
--     , background: Color.white900
--     , margin: "16, 16, 16, 16"
--     , cornerRadius: "9.0"
--     }
--     [ imageView
--         { imageWithFallback: fetchImage FF_ASSET $ if decrementMonthFlag then "ny_ic_chevron_left_black" else "ny_ic_chevron_left_grey"
--         , width: "24"
--         , height: "24"
--         , gravity: "center"
--         -- , onClick: push $ const if decrementMonthFlag then DecrementMonth decrementRes else NoAction
--         }
--     , linearLayout
--         { weight: "1.0"
--         , height: "24"
--         , width: "24"
--         , gravity: "center"
--         }
--         [ textView
--             { text: state.selectedTimeSpan.shortMonth
--             , textSize: "16"
--             -- , fontStyle: FontStyle.bold LanguageStyle
--             , color: Color.black800
--             }
--         , textView
--             { text: show state.selectedTimeSpan.year
--             , textSize: "16"
--             -- , fontStyle: FontStyle.bold LanguageStyle
--             , color: Color.black800
--             , margin: "5"
--             }
--         ]
--     , imageView
--         { imageWithFallback: fetchImage FF_ASSET $ if incrementMonthFlag then "ny_ic_chevron_right_black" else "ny_ic_chevron_right_grey"
--         , width: "28"
--         , height: "28"
--         , gravity: "center"
--         -- , onClick: push $ if incrementMonthFlag then IncrementMonth incrementRes else NoAction
--         }
--     ]

--   where
--   incrementMonthFlag = state.selectedTimeSpan.intMonth < state.futureLimit.intMonth && state.selectedTimeSpan.year <= state.futureLimit.year
--   -- incrementRes = EHU.incrementCalendarMonth state.selectedTimeSpan state.startDate state.endDate
--   decrementMonthFlag = state.selectedTimeSpan.intMonth > state.pastLimit.intMonth && state.selectedTimeSpan.year >= state.pastLimit.year

-- -- decrementRes = EHU.decrementCalendarMonth state.selectedTimeSpan state.startDate state.endDate

-- calendarView :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- calendarView push state =
--   linearLayout
--     { width: "wrap_content"
--     , height: "wrap_content"
--     , orientation: "horizontal"
--     -- , scrollBarX: false
--     }
--     [ linearLayout
--         { width: "match_parent"
--         , height: "wrap_content"
--         , orientation: "vertical"
--         , gravity: "center"
--         }
--         [ datePicker push state
--         ]
--     ]

-- datePicker :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- datePicker push state =
--   linearLayout
--     { width: "match_parent"
--     , height: "wrap_content"
--     , orientation: "vertical"
--     , gravity: "center"
--     }
--     [ weekView push state
--     , datesRowView push state
--     ]

-- weekView :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- weekView _ _ =
--   let
--     squareSize = (EHC.screenWidth unit - 32) / 7
--   in
--     linearLayout
--       { width: "wrap_content"
--       , height: "wrap_content"
--       , orientation: "horizontal"
--       , margin: "0, 0, 0, 5"
--       }
--       ( DA.mapWithIndex
--           ( \_ item ->
--               linearLayout
--                 { width: show $ if squareSize < 42 then squareSize else 42
--                 , gravity: "center"
--                 , padding: "0, 5, 0, 5"
--                 }
--                 [ textView
--                     { text: item
--                     , color: Color.black800
--                     , textSize: "14"
--                     }
--                 ]
--           )
--           [ "S", "M", "T", "W", "T", "F", "S" ]
--       )

-- datesRowView :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- datesRowView push state =
--   linearLayout
--     { width: "wrap_content"
--     , height: "wrap_content"
--     , orientation: "vertical"
--     , gravity: "center"
--     }
--     ( DA.mapWithIndex
--         ( \_ weekRow ->
--             linearLayout
--               { width: "match_parent"
--               , height: "match_parent"
--               , orientation: "horizontal"
--               }
--               [ dateView push state weekRow.week

--               ]
--         )
--         state.weeks
--     )

-- dateView :: (ComponentOutput -> Effect Unit) -> Config -> Array CalendarModalDateObject -> JSX
-- dateView _ state weekRow =
--   let
--     startDate = case state.startDate of
--       Nothing -> dummyDateItem
--       Just date -> date
--     endDate = case state.endDate of
--       Nothing -> dummyDateItem
--       Just date -> date
--     squareSize = (EHC.screenWidth unit - 32) / 7
--   in
--     linearLayout
--       { width: "match_parent"
--       , height: "wrap_content"
--       , margin: "0, 2, 0, 2"
--       }
--       ( DA.mapWithIndex
--           ( \_ item ->
--               linearLayout
--                 { width: show $ if squareSize < 42 then squareSize else 42
--                 , height: show $ if squareSize < 42 then squareSize else 42
--                 , gravity: "center"
--                 , cornerRadius:
--                     if item.isEnd then "50.0, false, true, true, false"
--                     else if item.isStart && endDate.date /= 0 then "50.0, true, false, false, true"
--                     else if item.isStart && endDate.date == 0 then "90.0, true, true, true, true"
--                     else "90.0, false, false, false, false"
--                 , background:
--                     if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.blue9000
--                     else if item.isInRange then Color.blue9000
--                     else Color.transparent
--                 -- , onClick: push $ const $ if (selectDateFlag item) then SelectDate (getSelectedDateRes item) else NoAction
--                 }
--                 [ textView
--                     { width: "match_parent"
--                     , height: "match_parent"
--                     , text: if item.date > 0 then show item.date else ""
--                     , color:
--                         if (item.utcDate < state.pastLimit.utcDate || item.utcDate > state.futureLimit.utcDate) then Color.black500
--                         else if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.white900
--                         else Color.black800
--                     , textSize: "14"
--                     , background:
--                         if ((item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate)) && item.date /= 0 then Color.blue800
--                         else Color.transparent
--                     , gravity: "center"
--                     , cornerRadius:
--                         if (item.utcDate == startDate.utcDate) || (item.utcDate == endDate.utcDate) then "90.0, true, true, true, true"
--                         else "90.0, false, false, false, false"
--                     }
--                 ]
--           )
--           weekRow
--       )

-- separator :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- separator _ _ =
--   linearLayout
--     { width: "match_parent"
--     , height: "1"
--     , background: Color.grey900
--     , margin: "0, 5, 0, 0"
--     }
--     []

-- selectedDateView :: (ComponentOutput -> Effect Unit) -> Config -> JSX
-- selectedDateView _ state =
--   let
--     startDate = case state.startDate of
--       Nothing -> dummyDateItem
--       Just date -> date
--     endDate = case state.endDate of
--       Nothing -> dummyDateItem
--       Just date -> date
--   in
--     linearLayout
--       { width: "match_parent"
--       , height: "wrap_content"
--       , orientation: "horizontal"
--       , gravity: "center"
--       , margin: "16, 16, 16, 16"
--       }
--       [ textView
--           { text: state.defaultMessage
--           , color: Color.black600
--           , textSize: "14"
--           -- , fontStyle: FontStyle.regular LanguageStyle
--           , visibility: if state.startDate == Nothing then "visible" else "gone"
--           }
--       , textView
--           { text: state.errorMessage
--           , color: Color.red
--           , textSize: "14"
--           -- , fontStyle: FontStyle.regular LanguageStyle
--           , visibility: if state.showError then "visible" else "gone"
--           }
--       , linearLayout
--           { width: "match_parent"
--           , height: "wrap_content"
--           , orientation: "horizontal"
--           , gravity: "center"
--           , visibility: if state.startDate == Nothing then "gone" else if state.showError then "gone" else "visible"
--           }
--           [ textView
--               { text: startDate.shortMonth <> " " <> (if startDate.date < 10 then "0" else "") <> show startDate.date <> ", " <> show startDate.year
--               , color: Color.black800
--               , textSize: "14"
--               -- , fontStyle: FontStyle.regular LanguageStyle
--               , gravity: "center"
--               }
--           , imageView
--               { imageWithFallback: fetchImage FF_ASSET "ny_ic_arrow_right"
--               , height: "match_parent"
--               , width: "16"
--               , margin: "8, 8, 8, 8"
--               , visibility: if state.endDate == Nothing then "gone" else "visible"
--               , gravity: "center"
--               }
--           , textView
--               { text: endDate.shortMonth <> " " <> (if endDate.date < 10 then "0" else "") <> show endDate.date <> ", " <> show endDate.year
--               , color: Color.black800
--               , textSize: "14"
--               -- , fontStyle: FontStyle.regular LanguageStyle
--               , visibility: if state.endDate == Nothing then "gone" else "visible"
--               , gravity: "center"
--               }
--           ]
--       ]