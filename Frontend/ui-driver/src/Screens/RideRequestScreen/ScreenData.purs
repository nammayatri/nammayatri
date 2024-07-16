module Screens.RideRequestScreen.ScreenData  where

import Data.Maybe
import Prelude
import Prelude
import Services.API
import Services.API

import Common.Types.App as CTA
import Prim.Boolean (True)
import Screens.Types as ST
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import PrestoDOM (Length(..), Margin(..), Padding(..), Prop, toPropValue)

type RideRequestScreenState = {
  data :: RideRequestScreenData,
  props :: RideRequestScreenProps

}
type RideRequestScreenData = {
  pillViewArray :: Array PillViewConfig 
  ,dayArray :: Array PillViewConfig
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
    carType : "",
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
    cornerRadius : "",
    imageType  : ""
}




type RideRequestScreenProps= {
  cardHeight :: Int,
  receivedResponse :: Boolean
}
type PillViewConfig ={
  index :: Int,
  rideType ::  Maybe (CTA.TripCategoryTag),
  pillViewText :: String,
  isSelected :: Boolean,
  activeColor :: String
}

initData :: RideRequestScreenState
initData = {
    data: {
      activeRideIndex : 0,
      activeDayIndex : 0,
      shimmerLoader: ST.AnimatingIn,
      pillViewArray : [
        {
          index : 0,
          pillViewText : "All",
          rideType :  Nothing,
          isSelected : true,
          activeColor : Color.black900
          }
        --   ,{
        --     index : 1,
        --     rideType :  Just CTA.OneWay,
        --   pillViewText : "Regular",
        --   isSelected : false,
        --   activeColor : Color.green900
        -- }
        ,{
          index : 1,
          rideType : Just CTA.InterCity,
          pillViewText : "Intercity",
          isSelected : false,
          activeColor : Color.blue800
        },
        {
          index : 2,
          rideType : Just CTA.Rental ,
          pillViewText : "Rental",
          isSelected : false,
          activeColor : Color.blueGreen
        }
        -- ,{
        --   index : 4,
        --   rideType : Just CTA.CrossCity ,
        --   pillViewText : "Scheduled",
        --   isSelected : false,
        --   activeColor :Color.black900
        -- }

            ]
       ,dayArray : [
              {
                index : 0,
                rideType : Nothing,
                pillViewText : "Today",
                isSelected : true,
                activeColor : Color.black900
                },{
                 index : 1,
                 rideType : Nothing,
                pillViewText : "Tommorow",
                isSelected : false,
                activeColor : Color.black900
              }

            ],
            offset : 0
            ,limit  :  "0"
            , tripCategory :  ""
            , date : (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "YYYY-MM-DD") 
            , resp : dummyResp
            , loaderButtonVisibility: false
            , scrollEnable : false
            , loadMoreDisabled : true
            , refreshLoader : false
            , filteredArr : []
           
            },
    props: {
      cardHeight : 0
    ,receivedResponse: false
    }
}


-- dummyIndividualCard :: ST.RideRequestCardState
-- dummyIndividualCard = {
--     date : "",
--     time : "",
--     source : "",
--     destination : "",
--     totalAmount : "",
--     cardVisibility : "",
--     shimmerVisibility : "",
--     carImage : "",
--     rideType : "",
--     carType : "",
--     srcLat :  0.00,
--     srcLong : 0.00,
--     desLat : 0.00,
--     desLong : 0.00,
--     id : ""
    
--   }

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
    carType : toPropValue "Non-AC-Mini",
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
    imageType : toPropValue ""
    }
]
-- date: toPropValue date
--           , time: toPropValue startTime
--           , distance: toPropValue distance
--           , source: toPropValue (decodeShortAddress (transformer ride.fromLocation))
--           , destination: toPropValue $ (decodeShortAddress (transformer (Location toLocation)))
--           , totalAmount: toPropValue $ ("₹ " <> show totalAmount)
--           , cardVisibility: toPropValue "visible"
--           , shimmerVisibility: toPropValue "gone"
--           , carImage: toPropValue $ totalAmount
--           , rideType: toPropValue $ show rideType
--           , carType: toPropValue $ show carType
--           , srcLat: toPropValue $ show srcLat
--           , srcLong: toPropValue $ show srcLong
--           , desLat: toPropValue $ show desLat
--           , desLong: toPropValue desLong
--           , id: toPropValue id
--           , image: toPropValue image
--           , visible: toPropValue visible
--           , color : toPropValue color
--           ,overlayVisiblity : toPropValue overlayVisiblity
--           ,visiblePill : toPropValue visiblePill

-- type RideRequestCardState =
--   {
--     date :: String,
--     time :: String,
--     source :: String,
--     distance :: String,
--     destination :: String,
--     totalAmount :: String,
--     cardVisibility :: String,
--     shimmerVisibility :: String,
--     carImage :: String,
--     rideType :: String,
--     carType :: String,
--     srcLat ::  Number,
--     srcLong :: Number,
--     desLat :: Number,
--     desLong :: Number,
--     id :: String,
--     image ::  String,
--     visible ::  Boolean,
--     color :: String,
--     overlayVisiblity :: String,
--     visiblePill :: String
--   }