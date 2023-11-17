{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.ComponentConfig where


import Common.Types.App (LazyCheck(..))
import Components.Calendar.Controller as CalendarConfig
import Components.ErrorModal as ErrorModal
import Components.ErrorModal.Controller (Action(..), Config)
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Components.RequestInfoCard as RequestInfoCard
import Data.Maybe (isJust)
import Engineering.Helpers.Utils (getCurrentDay)
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: Boolean -> PrimaryButtonConfig.Config
primaryButtonConfig isActive = let
    config' = PrimaryButtonConfig.config
    primaryButtonConfig' = config'
      { textConfig
        { text = "Buy Now"
        , accessibilityHint = (" : Buy Now")
        }
      , cornerRadius = 6.0
      , margin = Margin 16 24 16 16
      , isClickable = isActive
      , alpha = if isActive then 1.0 else 0.5
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.DriverEarningsScreenState -> GenericHeaderConfig.Config
genericHeaderConfig state = let
  config = GenericHeaderConfig.config
  genericHeaderConfig' = config 
    { height = WRAP_CONTENT
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = "Use Coins"
      , color = Color.darkCharcoal }
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left"
      } 
    , suffixImageConfig {
        visibility = GONE }
    }
  in genericHeaderConfig'

earningsPopupConfig :: ST.DriverEarningsScreenState -> PopUpModalConfig.Config
earningsPopupConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = true
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = case state.props.popupType of
                  ST.PLAN_PAID_POPUP -> "VERTICAL"
                  ST.SETUP_AUTOPAY_POPUP -> "VERTICAL"
                  ST.COINS_EXPIRING_POPUP -> "VERTICAL"
                  _ -> "HORIZONTAL"
    ,primaryText {
        text = case state.props.popupType of
                  ST.PLAN_PAID_POPUP -> "Daily Unlimited Plan paid by coins for 3 days"
                  ST.SETUP_AUTOPAY_POPUP -> "Setup Autopay to use coins"
                  ST.NO_COINS_POPUP -> "No coins available"
                  ST.COINS_EXPIRING_POPUP -> "Coins expiring!"
                  _ -> ""
      , margin = Margin 16 16 16 0
      , visibility = VISIBLE
      , color = Color.black800
      , textStyle = Heading2
     },
    option1 {
      text = case state.props.popupType of
                ST.PLAN_PAID_POPUP -> "View My Plan"
                ST.SETUP_AUTOPAY_POPUP -> "Setup Autopay"
                ST.NO_COINS_POPUP -> "Okay"
                ST.COINS_EXPIRING_POPUP -> "Use Coins Now"
                _ -> ""
    , color = Color.yellow900
    , background = Color.black900
    , visibility = true
    , margin = MarginTop 16
    , width = MATCH_PARENT
    },
    coverImageConfig {
      imageUrl =  case state.props.popupType of
        ST.PLAN_PAID_POPUP -> "ny_ic_plan_by_coin,"
        ST.SETUP_AUTOPAY_POPUP -> "ny_ic_plan_by_coin,"
        ST.NO_COINS_POPUP -> "ny_ic_no_coins,"
        ST.COINS_EXPIRING_POPUP -> ""
        _ -> ""
        
    , visibility = VISIBLE
    , width = V 280
    , height = V 210
    },
    secondaryText {
      text = case state.props.popupType of
                  ST.NO_COINS_POPUP -> "Earn coins by taking rides and referring the app to others"
                  ST.COINS_EXPIRING_POPUP -> "200 coins expiring in the next 2 days. Use them before they expire"
                  _ -> ""
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , visibility = case state.props.popupType of
                      ST.PLAN_PAID_POPUP -> VISIBLE
                      ST.COINS_EXPIRING_POPUP -> VISIBLE
                      _ -> GONE
      , textStyle = SubHeading2
      },
    option2 { 
      visibility = case state.props.popupType of
                      ST.NO_COINS_POPUP -> false
                      _ -> true
      , text = case state.props.popupType of
                  ST.PLAN_PAID_POPUP -> "Go Back"
                  ST.SETUP_AUTOPAY_POPUP -> "Maybe Later"
                  ST.COINS_EXPIRING_POPUP -> "Maybe Later"
                  _ -> ""
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0)
    },
    dismissPopup = true
    }
  in popUpConf'


calendarConfig :: ST.DriverEarningsScreenState -> CalendarConfig.Config
calendarConfig state = let 
  config = CalendarConfig.config
  calendarConfig' = config
    { weeks = state.props.calendarState.weeks
    , startDate = state.props.calendarState.startDate
    , endDate = state.props.calendarState.endDate
    , selectedTimeSpan = state.props.calendarState.selectedTimeSpan
    -- , showError = state.props.calendarState.showError
    -- , errorMessage = state.props.calendarState.errorMessage
    , primaryButtonConfig = calendarPrimaryButtonConfig state
    , cancelButtonConfig = calendarCancelButtonConfig state
    , defaultMessage = "Select Date"
    , pastLimit = {date : 15, isInRange : false, isStart : false, isEnd : false, utcDate : "2023-08-14T18:30:00.000Z", shortMonth : "Aug", year : 2023, intMonth : 7}
    , futureLimit = getCurrentDay ""
    , selectedDateColor = Color.blue800
    , dateInRangeColor = Color.blue9000
    , selectRange = false
    }
  in calendarConfig'

calendarPrimaryButtonConfig :: ST.DriverEarningsScreenState -> PrimaryButtonConfig.Config
calendarPrimaryButtonConfig state = 
    PrimaryButtonConfig.config {
      textConfig
        { text = getString APPLY
        }
      , cornerRadius = 6.0
      , margin = Margin 16 8 16 0
      , isClickable = isJust state.props.calendarState.startDate
      , alpha = if isJust state.props.calendarState.startDate then 1.0 else 0.5
      }

calendarCancelButtonConfig :: ST.DriverEarningsScreenState -> PrimaryButtonConfig.Config
calendarCancelButtonConfig state = 
    PrimaryButtonConfig.config {
      textConfig
        { text = getString CANCEL
        , color = Color.black650
        }
      , background = Color.white900
      , stroke = "1," <> Color.white900
      , margin = Margin 16 0 16 12
      , isClickable = isJust state.props.calendarState.startDate
      , alpha = if isJust state.props.calendarState.startDate then 1.0 else 0.5
      }

waitTimeInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
waitTimeInfoCardConfig _ = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = "What will my Coins be converted to?",
      color = Color.black800
    }
  , primaryText {
      text = "Yatri Coins will be converted into discounts that you can avail against your subscription plan.",
      padding = Padding 16 16 0 0,
      color = Color.black700
    }
  , imageConfig {
      imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_coins_info",
      height = V 130,
      width = V 130,
      padding = Padding 0 4 1 0
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20
    }
  }
  in requestInfoCardConfig'