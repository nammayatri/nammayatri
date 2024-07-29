module Screens.EarningsScreen.ScreenData where

import Common.Types.App (Price(..), Currency(..), Distance(..), DistanceUnit(..))
import Common.Types.App as Common
import Prelude
import Screens.EarningsScreen.Common.Types
import Screens.EarningsScreen.Common.Utils
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe
import PrestoDOM.List as List
import Styles.Colors as Color
import MerchantConfig.Types
import ConfigProvider
import JBridge
import Services.API 
import Screens.DriverEarningsScreen.ScreenData (dummyIndividualRideCard)
import Screens.Types (IndividualRideCardState)

data HistoryScreen = Payout | Ride

type State
  = { data :: Data
    , props :: Props
    }

type Data
  = { action :: Boolean
    , tipsRotation :: Number
    , adjustmentRotation :: Number
    , prevAdjustmentRotation :: Number
    , rideListItem :: Maybe List.ListItem
    , payoutListItem :: Maybe List.ListItem
    , config :: AppConfig
    , selectedDate :: Maybe RidesSummaryType
    , currentDate :: String
    , calendarState :: CalendarState
    , selectedDateRides :: Maybe (Array RideComponent)
    , selectedDatesRidesInfo :: Maybe (Array RidesInfo)
    , selectedRideItem :: IndividualRideCardState
    }

type Props
  = { actionProp :: String
    , showInfoView :: Boolean
    , rideDistanceInfoPopUp :: Boolean
    , selectedBarIndex :: Int
    , currentWeekMaxEarning :: Int
    , currWeekData :: Array WeeklyEarning
    , fromDate :: String
    , toDate :: String
    , isCurrentWeek :: Boolean
    , forwardBtnAlpha :: Number
    , totalWeeklyEarningsdata :: TotalWeeklyEarningsData
    , startBarAnim :: Boolean
    , isFromWeekly :: Boolean
    , isResetAnim :: Boolean
    }

type CalendarState = { 
  calendarPopup :: Boolean,
  endDate :: Maybe Common.CalendarModalDateObject,
  selectedTimeSpan :: Common.CalendarModalDateObject,
  startDate :: Maybe Common.CalendarModalDateObject,
  weeks  :: Array Common.CalendarModalWeekObject
}

type TotalWeeklyEarningsData = {
  fromDate :: String,
  toDate :: String,
  totalEarningsWithCurrency :: Common.Price,
  totalRides :: Int,
  totalDistanceTravelledWithUnit :: Common.Distance 
}

type RidesSummaryType = {
    earningsWithCurrency :: String,
    rideDate :: String,
    noOfRides :: String,
    rideDistanceWithUnit :: String
  }

type WeeklyEarning = { 
    earnings :: Int
  , earningsWithCurrency :: Price
  , rideDistance :: Int
  , rideDistanceWithUnit :: Distance
  , rideDate :: String
  , noOfRides :: Int
  , percentLength :: Number
  }
type ListProps = {
  serviceTier :: PropValue
, rideDate :: PropValue
, rideTime :: PropValue
, rideFare :: PropValue
, vehicleImage :: PropValue
, tagVisibilityPenality :: PropValue
, tagTextPenality :: PropValue
, tagVisibilityCancellation :: PropValue
, tagTextCancellation :: PropValue
, tagVisibilityTips :: PropValue
, tagTextTips :: PropValue
, vehicleImageVisibility :: PropValue
, rideFareColor :: PropValue
}

initialState :: State
initialState = 
  { data:
      { action: false
      , tipsRotation: 270.0
      , adjustmentRotation: 0.0
      , prevAdjustmentRotation: 0.0
      , rideListItem : Nothing
      , payoutListItem : Nothing
      , config : getAppConfig appConfig
      , selectedDate : Nothing
      , currentDate : formateDate getCurrentDate
      , selectedDateRides: Nothing
      , selectedDatesRidesInfo: Nothing
      , selectedRideItem: dummyIndividualRideCard
      , calendarState:
        { calendarPopup: false
        , endDate: Nothing
        , selectedTimeSpan: dummyDateItem
        , startDate: Just dummyDateItem
        , weeks: []
        }
      }
  , props:
      { actionProp: ""
      , showInfoView: false
      , rideDistanceInfoPopUp: false
      , selectedBarIndex: 0
      , currentWeekMaxEarning: 1500
      , currWeekData: []
      , fromDate: ""
      , toDate: ""
      , isCurrentWeek : false
      , startBarAnim: true 
      , forwardBtnAlpha: 1.0
      , totalWeeklyEarningsdata : dummyTotalWeeklyEarnings
      , isFromWeekly : false
      , isResetAnim: false
      }
  }

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }

dummyTotalWeeklyEarnings :: TotalWeeklyEarningsData
dummyTotalWeeklyEarnings = {
    fromDate : ""
  , toDate : ""
  , totalEarningsWithCurrency : dummyEarningsWithCurrency
  , totalRides : 0
  , totalDistanceTravelledWithUnit : dummyRideDistanceWithUnit
  }


dummyEarnings =
  [ { earnings: 1500
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 100.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 20.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 100
    , percentLength: 50.0
    }
  ]

dummyWeeklyEarnings =
  { earnings: 100
  , earningsWithCurrency : dummyEarningsWithCurrency
  , rideDistance: 100
  , rideDistanceWithUnit: dummyRideDistanceWithUnit
  , rideDate: "12"
  , noOfRides: -1
  , percentLength: 50.0
  }

dummyEarningsWithCurrency :: Price
dummyEarningsWithCurrency = {amount: 100.0, currency: INR}

dummyRideDistanceWithUnit :: Distance
dummyRideDistanceWithUnit = Distance {value: 0.0, unit: Kilometer}


dummyProps :: Array ListProps
dummyProps = [{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "visible"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
}]

dummyPayoutProps :: Array ListProps
dummyPayoutProps = [{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "visible"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
}]


listDatas :: Array RideComponent
listDatas =
  [ { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  , { serviceTierType: "Bridge XL"
    , date: "Today"
    , time: "7:45pm"
    , price: "$0"
    , isCancelledRide : false
    , tags:
        [ { background: Color.backDanger
          , color: Color.red900
          , text: "Passenger Cancelled"
          }
        , { background: "#F6F1FF"
          , color: Color.purple700
          , text: "Earned Penality"
          }
        ]
    }
  ]