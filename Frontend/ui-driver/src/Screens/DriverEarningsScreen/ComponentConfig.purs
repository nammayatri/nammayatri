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
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Prelude ((<>), (==), (*), show, not, (&&))
import Data.Int (toNumber)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color
import Storage (getValueToLocalStore, KeyStore(..))
import Mobility.Prelude

primaryButtonConfig :: Boolean -> PrimaryButtonConfig.Config
primaryButtonConfig isActive =
  PrimaryButtonConfig.config
    { textConfig
      { text = getString CONVERT
      , accessibilityHint = (" : Buy Now")
      }
    , cornerRadius = 6.0
    , margin = Margin 16 24 16 16
    , isClickable = isActive
    , alpha = if isActive then 1.0 else 0.5
    , enableRipple = isActive
    }

genericHeaderConfig :: ST.DriverEarningsScreenState -> GenericHeaderConfig.Config
genericHeaderConfig state =
  let
    cityConfig = HU.getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

    enableYatriCoins = state.data.config.feature.enableYatriCoins && cityConfig.enableYatriCoins

    headerText =
      if state.props.subView == ST.FAQ_VIEW then
        getString YATRI_COINS_FAQS
      else if state.props.subView == ST.FAQ_QUESTON_VIEW then
        ""
      else if not enableYatriCoins then
        getString EARNINGS
      else
        getString USE_COINS
  in
    GenericHeaderConfig.config
      { height = WRAP_CONTENT
      , padding = if state.props.subView == ST.EARNINGS_VIEW then Padding 16 16 16 16 else PaddingVertical 5 5
      , textConfig
        { text = headerText
        , color = Color.darkCharcoal
        }
      , prefixImageConfig
        { height = V 30
        , width = V 30
        , imageUrl = "ny_ic_chevron_left_black"
        , visibility = boolToVisibility enableYatriCoins
        , enableRipple = true
        , margin = Margin 8 8 8 8
        , layoutMargin = Margin 4 4 4 4
        }
      , suffixImageConfig
        { visibility = GONE
        }
      , margin = MarginBottom if enableYatriCoins then 0 else 24
      }

earningsPopupConfig :: ST.DriverEarningsScreenState -> PopUpModalConfig.Config
earningsPopupConfig state =
  let
    popupConfig = getPopupConfig state
  in
    PopUpModalConfig.config
      { cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor = Color.black9000
      , backgroundClickable = true
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = popupConfig.optionButtonOrientation
      , primaryText
        { text = popupConfig.primaryText
        , margin = Margin 16 16 16 0
        , visibility = VISIBLE
        , color = Color.black800
        , textStyle = Heading2
        }
      , option1
        { text = popupConfig.option1
        , color = Color.yellow900
        , background = Color.black900
        , visibility = true
        , margin = MarginTop 16
        , width = MATCH_PARENT
        }
      , coverImageConfig
        { imageUrl = popupConfig.coverImageConfig
        , visibility = VISIBLE
        , width = V 280
        , height = V 210
        }
      , secondaryText
        { text = popupConfig.secondaryText
        , color = Color.black700
        , margin = Margin 16 4 16 0
        , visibility = boolToVisibility popupConfig.secondaryTextVisibility
        , textStyle = SubHeading2
        }
      , option2
        { text = popupConfig.option2
        , visibility = popupConfig.option2Visibility
        , color = Color.black650
        , background = Color.white900
        , strokeColor = Color.white900
        , width = MATCH_PARENT
        , margin = Margin 0 0 0 0
        }
      , dismissPopup = true
      }

getPopupConfig :: ST.DriverEarningsScreenState -> { optionButtonOrientation :: String, primaryText :: String, secondaryText :: String, option1 :: String, option2 :: String, secondaryTextVisibility :: Boolean, option2Visibility :: Boolean, coverImageConfig :: String }
getPopupConfig state = case state.props.popupType of
  ST.COIN_TO_CASH_POPUP ->
    { optionButtonOrientation: "HORIZONTAL"
    , primaryText: getVarString WILL_BE_ADJUSTED_IN_YOUR_FUTURE_SUBSCRIPTION_DUES [ HU.getFixedTwoDecimals (state.data.coinConversionRate * toNumber state.data.coinsToUse) ]
    , secondaryText: ""
    , option1: getString OKAY
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: false
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_plan_by_coin"
    }
  ST.COIN_TO_CASH_FAIL_POPUP ->
    { optionButtonOrientation: "VERTICAL"
    , primaryText: getString FAILED_TO_USE_COINS_PLEASE_TRY_AGAIN_LATER
    , secondaryText: ""
    , option1: getString TRY_AGAIN
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: true
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_coin_to_cash_fail"
    }
  ST.NO_COINS_POPUP ->
    { optionButtonOrientation: "HORIZONTAL"
    , primaryText: getString NO_COINS_AVAILABLE
    , secondaryText: getString EARN_COINS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS
    , option1: getString OKAY
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: false
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_no_coins"
    }
  ST.COINS_EXPIRING_POPUP ->
    { optionButtonOrientation: "VERTICAL"
    , primaryText: getString COINS_EXPIRING
    , secondaryText: show state.data.expiringCoins <> getString COINS_EXPIRING_IN_THE_NEXT <> show state.data.expiringDays <> getString DAYS_USE_THEM_BEFORE_THEY_EXPIRE
    , option1: getString USE_COINS_NOW
    , option2: getString MAYBE_LATER
    , secondaryTextVisibility: false
    , option2Visibility: true
    , coverImageConfig: HU.fetchImage HU.FF_ASSET ""
    }
  _ -> { optionButtonOrientation: "HORIZONTAL", primaryText: "", secondaryText: "", option1: "", option2: "", secondaryTextVisibility: true, option2Visibility: false, coverImageConfig: HU.fetchImage HU.FF_ASSET ""}

calendarConfig :: ST.DriverEarningsScreenState -> CalendarConfig.Config
calendarConfig state =
  CalendarConfig.config
    { weeks = state.props.calendarState.weeks
    , startDate = state.props.calendarState.startDate
    , endDate = state.props.calendarState.endDate
    , selectedTimeSpan = state.props.calendarState.selectedTimeSpan
    , primaryButtonConfig = calendarPrimaryButtonConfig state
    , cancelButtonConfig = calendarCancelButtonConfig state
    , defaultMessage = getString SELECT_DATE
    , pastLimit = { date: 1, isInRange: false, isStart: false, isEnd: false, utcDate: "2022-11-01T18:30:00.000Z", shortMonth: "Nov", year: 2022, intMonth: 11 }
    , futureLimit = getCurrentDay false
    , selectedDateColor = Color.blue800
    , dateInRangeColor = Color.blue9000
    , selectRange = false
    }

calendarPrimaryButtonConfig :: ST.DriverEarningsScreenState -> PrimaryButtonConfig.Config
calendarPrimaryButtonConfig state =
  PrimaryButtonConfig.config
    { textConfig
      { text = getString APPLY
      }
    , cornerRadius = 6.0
    , margin = Margin 16 8 16 0
    , isClickable = isJust state.props.calendarState.startDate
    , alpha = if isJust state.props.calendarState.startDate then 1.0 else 0.5
    }

calendarCancelButtonConfig :: ST.DriverEarningsScreenState -> PrimaryButtonConfig.Config
calendarCancelButtonConfig state =
  PrimaryButtonConfig.config
    { textConfig
      { text = getString CANCEL
      , color = Color.black650
      }
    , background = Color.white900
    , stroke = "1," <> Color.white900
    , margin = Margin 16 0 16 12
    , isClickable = isJust state.props.calendarState.startDate
    , alpha = if isJust state.props.calendarState.startDate then 1.0 else 0.5
    }

coinsInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
coinsInfoCardConfig _ =
  RequestInfoCard.config
    { title
      { text = getString WHAT_WILL_MY_COINS_BE_CONVERTED_TO
      , color = Color.black800
      }
    , primaryText
      { text = getString YATRI_COINS_USAGE_POPUP
      , padding = Padding 16 16 0 0
      , color = Color.black700
      }
    , imageConfig
      { imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_coins_info"
      , height = V 130
      , width = V 130
      , padding = PaddingRight 1
      }
    , buttonConfig
      { text = getString GOT_IT
      , padding = PaddingVertical 16 20
      }
    }

errorModalConfig :: ST.DriverEarningsScreenState -> ErrorModal.Config
errorModalConfig state =
  ErrorModal.config
    { imageConfig
      { imageUrl =
        HU.fetchImage HU.FF_ASSET
          if state.props.subView == ST.EARNINGS_VIEW then
            "ny_ic_no_rides_history"
          else
            "ny_ic_no_coins_history"
      , height = V if state.props.subView == ST.EARNINGS_VIEW then 110 else 115
      , width = V if state.props.subView == ST.EARNINGS_VIEW then 124 else 200
      , margin = MarginBottom 61
      }
    , errorConfig
      { text = getString if state.props.subView == ST.EARNINGS_VIEW then NO_RIDE_HISTORY_AVAILABLE else COMPLETE_FIRST_RIDE_TO_UNLOCK_COINS
      , margin = MarginBottom 7
      , color = Color.black900
      }
    , errorDescriptionConfig
      { text = getString if state.props.subView == ST.EARNINGS_VIEW then YOU_HAVE_NOT_COMPLETED_A_RIDE_YET else EARN_COINS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS
      , color = Color.black700
      }
    , buttonConfig
      { visibility = GONE
      }
    , height = MATCH_PARENT
    , background = Color.transparent
    }
