{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerReferralTrackerScreen.ComponentConfig where

import Common.Types.App (LazyCheck(..))
import Components.Calendar.Controller as CalendarConfig
import Components.ErrorModal as ErrorModal
import Components.ErrorModal.Controller (Action(..), Config)
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryButton.View as PrimaryButton
import Components.ReferralStepsView as ReferralStepsView
import Data.Maybe
import Engineering.Helpers.Utils (getCurrentDay)
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==), (*), (&&), ($), (||), (-), show, not, unit)
import Data.Int (toNumber)
import Data.Array ((!!), take, drop, any)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color
import Storage (getValueToLocalStore, KeyStore(..))
import Mobility.Prelude
import LocalStorage.Cache (getValueFromCache)
import JBridge as JB
import Engineering.Helpers.Utils (getFixedTwoDecimals)
import Screens.CustomerReferralTrackerScreen.Types
import Mobility.Prelude
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle

calendarConfig :: CustomerReferralTrackerScreenState -> CalendarConfig.Config
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

calendarPrimaryButtonConfig :: CustomerReferralTrackerScreenState -> PrimaryButton.Config
calendarPrimaryButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString APPLY
      }
    , cornerRadius = 6.0
    , margin = Margin 16 8 16 0
    , isClickable = isJust state.props.calendarState.startDate
    , alpha = if isJust state.props.calendarState.startDate then 1.0 else 0.5
    , enableRipple = isJust state.props.calendarState.startDate
    }

calendarCancelButtonConfig :: CustomerReferralTrackerScreenState -> PrimaryButton.Config
calendarCancelButtonConfig state =
  PrimaryButton.config
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

genericHeaderConfig :: CustomerReferralTrackerScreenState -> GenericHeaderConfig.Config
genericHeaderConfig state =
  let
    headerText = case state.data.currentStage of 
                    UPIDetails -> getString UPI_DETAILS 
                    TransactionHistory -> getString TRANSACTION_DETAILS
                    _ -> getString CUSTOMER_REFERRAL_TRACKER
  in
    GenericHeaderConfig.config
      { height = WRAP_CONTENT
      , padding = Padding 8 16 16 16
      , textConfig
        { text = headerText
        , color = Color.darkCharcoal
        }
      , prefixImageConfig
        { height = V 30
        , width = V 30
        , imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_chevron_left_black"
        , enableRipple = true
        , margin = MarginRight 8
        , layoutMargin = MarginBottom 0
        }
      , suffixImageConfig
        { height = V 24
        , width = V 24
        , imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_more_vertical"
        , enableRipple = false
        , visibility = boolToVisibility $ (any (_ == state.data.currentStage) [Tracker, ReferralSteps]) || (state.data.currentStage == UPIDetails && isJust state.data.upiID)
        }
      }

addUPIDetailsButtonConfig :: CustomerReferralTrackerScreenState -> PrimaryButton.Config
addUPIDetailsButtonConfig state = 
  PrimaryButton.config
    { textConfig
      { text = getString $ PAY_TO_ADD_UPI (state.data.config.currency <> (show state.data.registrationAmount))
      , color = Color.yellow900
      }
    , id = "AddUPIButton"
    , enableLoader = JB.getBtnLoader "AddUPIButton"
    , enableRipple = true
    }

deleteUPIButtonConfig :: CustomerReferralTrackerScreenState -> PrimaryButton.Config
deleteUPIButtonConfig state = 
  PrimaryButton.config
    { height = V 48
    , width = MATCH_PARENT
    , background = Color.red900
    , margin = MarginBottom 8
    , id = "deleteUPIButton"
    , enableRipple = true
    , enableLoader = JB.getBtnLoader "deleteUPIButton"
    , rippleColor = Color.rippleShade
    , lottieConfig 
      { 
        lottieURL = "primary_button_loader_white.json"
      }
    , textConfig
      { text = getString YES_DELETE
      , color = Color.white900
      , textStyle = FontStyle.SubHeading3
      }
    }
  
paymentSuccessConfig :: CustomerReferralTrackerScreenState -> PopUpModal.Config
paymentSuccessConfig _ =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText
          { text = getString PAYMENT_SUCCESSFUL
          , margin = MarginLeft 0
          , textStyle = Heading2
          }
        , coverImageConfig
          { imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_check_circle_green"
          , height = V 114
          , width = V 114
          , visibility = VISIBLE
          , margin = MarginBottom 32
          }
        , option2
          { visibility = false
          }
        , option1 { 
            text = getString GOT_IT
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , secondaryText { 
            visibility = GONE 
          }
        , backgroundColor = Color.transparent
        , gravity = CENTER
        , padding = Padding 16 32 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

paymentFailedConfig :: CustomerReferralTrackerScreenState -> PopUpModal.Config
paymentFailedConfig _ =
  let
    config = PopUpModal.config
    requestInfoCardConfig' =
      config
        { primaryText 
          { text = getString PAYMENT_FAILED
          , margin = MarginLeft 0
          , textStyle = Heading2
          }
        , secondaryText 
          { text = getString YOUR_PAYMENT_WAS_UNSUCCESSFUL
          , margin = MarginTop 8
          , textStyle = SubHeading2
          }
        , coverImageConfig
          { imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_failed"
          , height = V 200
          , width = MATCH_PARENT
          , visibility = VISIBLE
          , margin = MarginBottom 24
          }
        , option1 { 
            text = getString RETRY_PAYMENT_STR
          , width = MATCH_PARENT
          , margin = MarginTop 24
          , background = Color.black900
          , color = Color.yellow900
          }
        , option2
          { text = getString CLOSE
          , width = MATCH_PARENT
          , margin = MarginTop 8
          , background = Color.transparent
          , color = Color.black650
          , strokeColor = Color.transparent
          }
        , backgroundColor = Color.transparent
        , gravity = CENTER
        , padding = Padding 16 32 16 16
        , cornerRadius = Corners 16.0 true true true true
        , margin = MarginHorizontal 16 16
        , buttonLayoutMargin = MarginLeft 0
        , optionButtonOrientation = "VERTICAL"
        , dismissPopup = true
        }
  in
    requestInfoCardConfig'

referralStepsViewConfig :: CustomerReferralTrackerScreenState -> ReferralStepsView.Config
referralStepsViewConfig state = ReferralStepsView.config{referralSteps = getStepsConfig state.data.selectedItem, highlightTitle = isNothing state.data.selectedItem, heading = getString $ REFERRAL_FIRST_RIDE_DESCRIPTION $ state.data.config.currency <> (show state.data.referralRewardAmountPerRide)}

getStepsConfig :: Maybe DailyEarning -> Array ReferralStepsView.StepConfig
getStepsConfig mbEarning = case mbEarning of 
                            Just earning -> case earning.status of 
                                            Verifying -> (take 2 $ doneStepsData FunctionCall) <> [fromMaybe (dummyStepConfig FunctionCall) ((inProgressData FunctionCall) !! 2)] <> drop 3 (pendingStepsData FunctionCall)
                                            Processing -> (take 3 $ doneStepsData FunctionCall) <> [fromMaybe (dummyStepConfig FunctionCall) ((inProgressData FunctionCall) !! 3)] <> drop 4 (pendingStepsData FunctionCall)
                                            _ -> []
                            Nothing -> pendingStepsData FunctionCall


pendingStepsData :: LazyCheck -> Array ReferralStepsView.StepConfig
pendingStepsData dummy = 
  [ { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_corner_up_right_grey"
    , title : getString REFER_CUSTOMER
    , background : Color.grey700
    , status : ReferralStepsView.Pending
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_car_outline_grey"
    , title : getString CUSTOMER_COMPLETED_FIRST_RIDE
    , background : Color.grey700
    , status : ReferralStepsView.Pending
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_search_grey"
    , title : getString VERIFYING
    , background : Color.grey700
    , status : ReferralStepsView.Pending
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_more_horizontal_grey"
    , title : getString PROCESSING
    , background : Color.grey700
    , status : ReferralStepsView.Pending
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_payment_credited_grey"
    , title : getString PAYMENT_CREDITED
    , background : Color.grey700
    , status : ReferralStepsView.Pending
    }
  ]

doneStepsData :: LazyCheck -> Array ReferralStepsView.StepConfig
doneStepsData dummy = 
  [ { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_corner_up_right_white"
    , title : getString REFER_CUSTOMER
    , background : Color.green900
    , status : ReferralStepsView.Done
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_car_outline_white"
    , title : getString CUSTOMER_COMPLETED_FIRST_RIDE
    , background : Color.green900
    , status : ReferralStepsView.Done
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_search_white"
    , title : getString VERIFYING
    , background : Color.green900
    , status : ReferralStepsView.Done
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_more_horizontal_white"
    , title : getString PROCESSING
    , background : Color.green900
    , status : ReferralStepsView.Done
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_payment_credited_grey"
    , title : getString PAYMENT_CREDITED
    , background : Color.green900
    , status : ReferralStepsView.Done
    }
  ]

inProgressData :: LazyCheck -> Array ReferralStepsView.StepConfig
inProgressData dummy =
  [ { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_corner_up_right_white"
    , title : getString REFER_CUSTOMER
    , background : Color.blue800
    , status : ReferralStepsView.InProgress
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_car_outline_white"
    , title : getString CUSTOMER_COMPLETED_FIRST_RIDE
    , background : Color.blue800
    , status : ReferralStepsView.InProgress
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_search_white"
    , title : getString VERIFYING
    , background : Color.orange900
    , status : ReferralStepsView.InProgress
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_more_horizontal_white"
    , title : getString PROCESSING
    , background : Color.blue800
    , status : ReferralStepsView.InProgress
    }
  , { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_payment_credited_grey"
    , title : getString PAYMENT_CREDITED
    , background : Color.blue800
    , status : ReferralStepsView.InProgress
    }
  ]

dummyStepConfig :: LazyCheck -> ReferralStepsView.StepConfig
dummyStepConfig dummy = 
  { icon : HU.fetchImage HU.COMMON_ASSET "ny_ic_corner_up_right_grey"
  , title : getString REFER_CUSTOMER
  , background : Color.grey700
  , status : ReferralStepsView.Pending
  }