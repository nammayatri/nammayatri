module Screens.RideRequestScreen.ScreenData  where

import Data.Maybe
import Prelude
import Services.API 
import Common.Types.App as CTA
import Prim.Boolean (True)
import Screens.Types as ST
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Language.Types (STR(..)) as LType
import Language.Strings (getString)
import ConfigProvider
import PrestoDOM (Length(..), Margin(..), Padding(..), Prop, toPropValue)
import MerchantConfig.Types (AppConfig)
type RideRequestScreenState = {
  data :: RideRequestScreenData,
  props :: RideRequestScreenProps

}
type RideRequestScreenData = {
  pillViewArray :: Array PillViewConfig 
  ,offset :: Int
  ,limit  ::  String
  ,tripCategory ::  String
  ,resp :: ScheduledBookingListResponse
  ,date :: String
  ,filteredArr  ::  Array ScheduleBooking
  ,activeRideIndex :: Int 
  ,activeDayIndex :: Int
  ,loaderButtonVisibility :: Boolean
  ,loadMoreDisabled :: Boolean
  ,shimmerLoader :: ST.AnimationState
  ,refreshLoader :: Boolean
  ,scrollEnable :: Boolean
  ,currCard :: BookingAPIEntity
  ,fareDetails :: Array RateCardItem
  ,vehicleType :: String
  ,driverLat :: Maybe String
  ,driverLong :: Maybe String
  ,config :: AppConfig
}

dummyResp = ScheduledBookingListResponse {
  bookings : []
}

dummy_Resp ::  ST.RideRequestCardState
dummy_Resp =   {
    date : "",
    time : "",
    source : "",
    distance : "0",
    destination : "",
    totalAmount : "",
    cardVisibility : "",
    shimmerVisibility : "",
    carImage : "",
    rideType : "",
    vehicleType : "",
    srcLat :  0.0000,
    srcLong :  0.0000,
    desLat :  0.0000,
    desLong :  0.0000,
    id : "",
    image : "",
    visible : true,
    pillColor : "",
    overlayVisiblity : "",
    visiblePill : "",
    cornerRadius: "",
    imageType: "",
    estimatedDuration: ""
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  area: Default,
  createdAt : "2016-07-22T00:00:00Z",
  currency : INR,
  disabilityTag : Nothing,
  distanceToPickup : Nothing,
  estimatedDistance : Nothing,
  estimatedDuration  : Nothing,
  estimatedFare : 0.0,
  fareParams : FareParameters {
    baseFare : 0.0,
    congestionCharge : Nothing,
    currency : INR,
    customerCancellationDues : Nothing,
    customerExtraFee : Nothing,
    driverSelectedFare : Nothing,
    fareParametersDetails : FareParametersDetails{
      contents : Content {
          currency : INR,
          deadKmFare : Nothing,
          distBasedFare : Nothing,
          distanceUnit : Nothing,
          extraDistance : Nothing, 
          extraDuration : Nothing,
          timeBasedFare : Nothing
      },
      tag :Nothing
    },
    govtCharges : Nothing,
    id : "",
    nightShiftCharge : Nothing,
    nightShiftRateIfApplies : Nothing,
    parkingCharge : Nothing,
    rideExtraTimeFare : Nothing,
    serviceCharge : Nothing,
    tollCharges : Nothing,
    updatedAt : "2016-07-22T00:00:00Z",
    waitingCharge : Nothing
  },
  fromLocation : Location{
    address : LocationAddress{
          area :Nothing,
          areaCode :Nothing,
          building :Nothing,
          city :Nothing,
          country : Nothing,
          state : Nothing,
          street : Nothing,
          door : Nothing,
          fullAddress :Nothing
    },
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :""
  },
  id : "",
  isScheduled : true,
  maxEstimatedDistance : Nothing,
  returnTime :Nothing,
  roundTrip : Nothing,
  isAirConditioned : Nothing,
  specialZoneOtpCode : Nothing,
  startTime : "2016-07-22T00:00:00Z",
  status : CTA.UPCOMING,
  stopLocationId : Nothing,
  toLocation : Nothing,
  tollNames : Nothing,
  tripCategory : CTA.TripCategory {
    contents :Nothing,
    tag : CTA.OneWay
  },
  updatedAt : "2016-07-22T00:00:00Z",
  vehicleServiceTier : COMFY,
  vehicleServiceTierAirConditioned : Nothing,
  vehicleServiceTierName : "",
  vehicleServiceTierSeatingCapacity : Nothing
}




type RideRequestScreenProps= {
  cardHeight :: Int,
  receivedResponse :: Boolean,
  shouldCall :: Boolean,
  noLocationFlag :: Boolean
}
type PillViewConfig ={
  rideType ::  Maybe (CTA.TripCategoryTag),
  pillViewText :: String,
  isSelected :: Boolean,
  activeColor :: String
}

initData :: String -> RideRequestScreenState
initData _ = {
    data: {
      config : getAppConfig appConfig,
      activeRideIndex : 0,
      activeDayIndex : 0,
      shimmerLoader: ST.AnimatingIn,
      pillViewArray : []
            ,offset : 0
            ,limit  :  "0"
            , tripCategory :  ""
            , date : (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "YYYY-MM-DD") 
            , resp : dummyResp
            , loaderButtonVisibility: false
            , scrollEnable : false
            , loadMoreDisabled : true
            , refreshLoader : false
            , filteredArr : []
            , currCard : dummyAPI
            , fareDetails : []
            , vehicleType : ""
            , driverLat : Nothing
            , driverLong : Nothing
            },
    props: {
      cardHeight : 0
    ,receivedResponse: false
    , shouldCall : true
    , noLocationFlag : false
    }
}


cardData :: Array ST.RideCardItemState
cardData = [
  {
    date :  toPropValue "13 Apr,2024",
    time : toPropValue "7 PM",
    source : toPropValue "Koramangla",
    distance : toPropValue "120 KM",
    destination : toPropValue "BTM",
    totalAmount : toPropValue "2000",
    cardVisibility :  toPropValue "visible",
    shimmerVisibility : toPropValue "",
    carImage : toPropValue "",
    rideType : toPropValue  "Regular",
    vehicleType : toPropValue "Non-AC-Mini",
    srcLat :   toPropValue 0.0000,
    srcLong :  toPropValue 0.0000,
    desLat :  toPropValue 0.0000,
    desLong : toPropValue  0.0000,
    id : toPropValue "",
    image : toPropValue "ny_ic_auto_side_view",
    visible : toPropValue false,
    pillColor : toPropValue "",
    overlayVisiblity :  toPropValue "gone",
    visiblePill : toPropValue "",
    cornerRadius : toPropValue "",
    imageType : toPropValue "",
    estimatedDuration : toPropValue "" 
    }
]

rideTypePills :: Boolean -> Array PillViewConfig
rideTypePills includeIntercity =
    ([ { rideType: Just CTA.Rental
              , pillViewText: getString LType.RENTAL
              , isSelected: false
              , activeColor: Color.blueGreen
              }
      ])
          <> (if includeIntercity then
        [ { rideType: Just CTA.InterCity
          , pillViewText: getString LType.INTERCITY
          , isSelected: false
          , activeColor: Color.blue800
          }
        ]
      else
        [])