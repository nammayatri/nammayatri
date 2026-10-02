{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreen.ComponentConfig where

import Mobility.Prelude

import Common.Types.App (LazyCheck(..))
import Components.Calendar.Controller as CalendarConfig
import Components.ErrorModal as ErrorModal
import Components.ErrorModal.Controller (Action(..), Config)
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Components.RequestInfoCard as RequestInfoCard
import Data.Int (toNumber)
import Data.Maybe (isJust)
import Engineering.Helpers.Utils (getCurrentDay)
import Engineering.Helpers.Utils (getFixedTwoDecimals)
import Font.Style (Style(..))
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import Prelude ((<>), (==), (*), show, not, (&&), ($))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Data.Array as DA
import Font.Style as FontStyle

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
        getString YATRI_POINTS_FAQS
      else if state.props.subView == ST.FAQ_QUESTON_VIEW then
        ""
      else if not enableYatriCoins then
        getString EARNINGS
      else
        getString USE_POINTS
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
    , primaryText: getVarString WILL_BE_ADJUSTED_IN_YOUR_FUTURE_SUBSCRIPTION_DUES [ getFixedTwoDecimals (state.data.coinConversionRate * toNumber state.data.coinsToUse) ]
    , secondaryText: ""
    , option1: getString OKAY
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: false
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_plan_by_coin_v2"
    }
  ST.COIN_TO_CASH_FAIL_POPUP ->
    { optionButtonOrientation: "VERTICAL"
    , primaryText: getString FAILED_TO_USE_POINTS_PLEASE_TRY_AGAIN_LATER
    , secondaryText: ""
    , option1: getString TRY_AGAIN
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: true
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_coin_to_cash_fail_v2"
    }
  ST.NO_COINS_POPUP ->
    { optionButtonOrientation: "HORIZONTAL"
    , primaryText: getString NO_POINTS_AVAILABLE
    , secondaryText: getString $ EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS"
    , option1: getString OKAY
    , option2: getString GO_BACK
    , secondaryTextVisibility: false
    , option2Visibility: false
    , coverImageConfig: HU.fetchImage HU.FF_ASSET "ny_ic_no_coins"
    }
  ST.COINS_EXPIRING_POPUP ->
    { optionButtonOrientation: "VERTICAL"
    , primaryText: getString POINTS_EXPIRING
    , secondaryText: show state.data.expiringCoins <> getString POINTS_EXPIRING_IN_THE_NEXT <> show state.data.expiringDays <> getString DAYS_USE_THEM_BEFORE_THEY_EXPIRE
    , option1: getString USE_POINTS_NOW
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
    , enableRipple = isJust state.props.calendarState.startDate
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

coinsInfoCardConfig :: LazyCheck -> Int -> RequestInfoCard.Config
coinsInfoCardConfig _ stepFunction =
  RequestInfoCard.config
    { title
      { text = getString WHAT_WILL_MY_POINTS_BE_CONVERTED_TO
      , color = Color.black800
      }
    , primaryText
      { text = getString $ YATRI_POINTS_USAGE_POPUP "YATRI_POINTS_USAGE_POPUP"
      , padding = Padding 16 16 0 0
      , color = Color.black700
      }
    , secondaryText 
      { text = getString $ YATRI_POINTS_USAGE_SECONDARY (show $ stepFunction) (show 1),
        textStyle = FontStyle.Body1,
        padding = Padding 16 16 26 0,
        visibility = VISIBLE,
        margin = MarginRight 116
      }
    , imageConfig
      { imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_coins_info"
      , height = V 130
      , width = V 130
      , padding = PaddingRight 1
      , visibility = VISIBLE
      }
    , buttonConfig
      { text = getString GOT_IT
      , padding = PaddingVertical 16 20
      }
    }

errorModalConfig :: ST.DriverEarningsScreenState -> ErrorModal.Config
errorModalConfig state =
  let vehicleVariantLocalStore = getValueFromCache (show VEHICLE_VARIANT) JB.getKeyInSharedPrefKeys
      isVehicleRickshaw = vehicleVariantLocalStore `DA.elem` ["EV_AUTO_RICKSHAW", "AUTO_RICKSHAW"]
      isVehicleBike = vehicleVariantLocalStore == "BIKE"
      isVehicleAmbulance = vehicleVariantLocalStore `DA.elem` ["AMBULANCE", "AMBULANCE_TAXI", "AMBULANCE_TAXI_OXY", "AMBULANCE_AC", "AMBULANCE_AC_OXY", "AMBULANCE_VENTILATOR"]
      isVehicleTruck = vehicleVariantLocalStore == "DELIVERY_LIGHT_GOODS_VEHICLE"
      isVehicleHeritageCab = vehicleVariantLocalStore == "HERITAGE_CAB"
  in ErrorModal.config
    { imageConfig
      { imageUrl =
        if isVehicleRickshaw 
        then
            HU.fetchImage HU.FF_ASSET $
                if state.props.subView == ST.EARNINGS_VIEW then "ny_ic_no_rides_history"
                else "ny_ic_no_coins_history"
        else if isVehicleBike then HU.fetchImage HU.FF_ASSET "ny_ic_no_rides_history_bike"
        else if isVehicleAmbulance then HU.fetchImage HU.FF_ASSET "ny_ic_no_rides_history_ambulance"
        else if isVehicleTruck then HU.fetchImage HU.FF_ASSET "ny_ic_no_rides_history_truck"
        else if isVehicleHeritageCab then HU.fetchImage HU.FF_ASSET "ny_ic_no_rides_history_heritage_cab"
        else "ny_ic_no_rides_history_cab,https://assets.moving.tech/beckn/jatrisaathi/driver/images/ny_ic_no_rides_history_cab.png"
      , height = V if state.props.subView == ST.EARNINGS_VIEW then 110 else 115
      , width = V if state.props.subView == ST.EARNINGS_VIEW then 124 else 200
      , margin = MarginBottom 61
      }
    , errorConfig
      { text = getString if state.props.subView == ST.EARNINGS_VIEW then NO_RIDE_HISTORY_AVAILABLE else COMPLETE_FIRST_RIDE_TO_UNLOCK_POINTS
      , margin = MarginBottom 7
      , color = Color.black900
      }
    , errorDescriptionConfig
      { text = if state.props.subView == ST.EARNINGS_VIEW then getString YOU_HAVE_NOT_COMPLETED_A_RIDE_YET else getString $ EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS"
      , color = Color.black700
      }
    , buttonConfig
      { visibility = GONE
      }
    , height = MATCH_PARENT
    , background = Color.transparent
    }
