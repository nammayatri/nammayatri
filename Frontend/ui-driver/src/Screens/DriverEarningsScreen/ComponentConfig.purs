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
import Prelude ((<>), (==), (*), show, not)
import Data.Int (toNumber)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: Boolean -> PrimaryButtonConfig.Config
primaryButtonConfig isActive = let
    config' = PrimaryButtonConfig.config
    primaryButtonConfig' = config'
      { textConfig
        { text = getString BUY_NOW
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
        text = if not state.data.config.coinsConfig.enableYatriCoins then getString EARNINGS else getString USE_COINS
      , color = Color.darkCharcoal }
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left"
      , visibility = if state.data.config.coinsConfig.enableYatriCoins then VISIBLE else GONE
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
                  ST.COIN_TO_CASH_POPUP -> "HORIZONTAL"
                  ST.COIN_TO_CASH_FAIL_POPUP -> "VERTICAL"
                  ST.COINS_EXPIRING_POPUP -> "VERTICAL"
                  _ -> "HORIZONTAL"
    ,primaryText {
        text = case state.props.popupType of
                  ST.COIN_TO_CASH_POPUP -> show (state.data.coinConversionRate * (toNumber state.data.coinsToUse)) <> " will be adjusted against your subscription plan"
                  ST.COIN_TO_CASH_FAIL_POPUP -> getString FAILED_TO_USE_COINS_PLEASE_TRY_AGAIN_LATER
                  ST.NO_COINS_POPUP -> getString NO_COINS_AVAILABLE
                  ST.COINS_EXPIRING_POPUP -> getString COINS_EXPIRING
                  _ -> ""
      , margin = Margin 16 16 16 0
      , visibility = VISIBLE
      , color = Color.black800
      , textStyle = Heading2
     },
    option1 {
      text = case state.props.popupType of
                ST.COIN_TO_CASH_POPUP -> getString OKAY
                ST.COIN_TO_CASH_FAIL_POPUP -> getString TRY_AGAIN
                ST.NO_COINS_POPUP -> getString OKAY
                ST.COINS_EXPIRING_POPUP -> getString USE_COINS_NOW
                _ -> ""
    , color = Color.yellow900
    , background = Color.black900
    , visibility = true
    , margin = MarginTop 16
    , width = MATCH_PARENT
    },
    coverImageConfig {
      imageUrl =  case state.props.popupType of
        ST.COIN_TO_CASH_POPUP -> "ny_ic_plan_by_coin,"
        ST.COIN_TO_CASH_FAIL_POPUP -> "ny_ic_coin_to_cash_fail,"
        ST.NO_COINS_POPUP -> "ny_ic_no_coins,"
        ST.COINS_EXPIRING_POPUP -> ""
        _ -> ""
        
    , visibility = VISIBLE
    , width = V 280
    , height = V 210
    },
    secondaryText {
      text = case state.props.popupType of
                  ST.NO_COINS_POPUP -> getString EARN_COINS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS
                  ST.COINS_EXPIRING_POPUP -> show state.data.expiringCoins <> getString COINS_EXPIRING_IN_THE_NEXT <> show state.data.expiringDays <> getString DAYS_USE_THEM_BEFORE_THEY_EXPIRE
                  _ -> ""
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , visibility = case state.props.popupType of
                      ST.COIN_TO_CASH_POPUP -> VISIBLE
                      ST.COINS_EXPIRING_POPUP -> VISIBLE
                      _ -> GONE
      , textStyle = SubHeading2
      },
    option2 { 
      visibility = case state.props.popupType of
                      ST.NO_COINS_POPUP -> false
                      ST.COIN_TO_CASH_POPUP -> false
                      _ -> true
      , text = case state.props.popupType of
                  ST.COIN_TO_CASH_POPUP -> getString GO_BACK
                  ST.COIN_TO_CASH_FAIL_POPUP -> getString GO_BACK
                  ST.COINS_EXPIRING_POPUP -> getString MAYBE_LATER
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
    , primaryButtonConfig = calendarPrimaryButtonConfig state
    , cancelButtonConfig = calendarCancelButtonConfig state
    , defaultMessage = getString SELECT_DATE
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
      text = getString WHAT_WILL_MY_COINS_BE_CONVERTED_TO,
      color = Color.black800
    }
  , primaryText {
      text = getString YATRI_COINS_USAGE_POPUP,
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


errorModalConfig :: ST.DriverEarningsScreenState -> ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = if state.props.subView == ST.EARNINGS_VIEW then "ny_ic_no_rides_history,"
                   else  "ny_ic_no_coins_history,"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 61)
      }
    , errorConfig {
        text = if state.props.subView == ST.EARNINGS_VIEW then getString NO_RIDE_HISTORY_AVAILABLE else getString COMPLETE_FIRST_RIDE_TO_UNLOCK_COINS
      , margin = (MarginBottom 7)  
      , color = Color.black900
      }
    , errorDescriptionConfig {
        text = if state.props.subView == ST.EARNINGS_VIEW then getString YOU_HAVE_NOT_COMPLETED_A_RIDE_YET else getString EARN_COINS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS
      , color = Color.black700
      }
    , buttonConfig {
      visibility  = GONE
      }
    }
    in errorModalConfig' 