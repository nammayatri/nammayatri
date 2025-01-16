module Screens.RideSummaryScreen.ComponentConfig where

import Prelude
import Components.SourceToDestination as SourceToDestination
import Components.RideSummaryCard as RideSummaryCard
import Screens.RideSummaryScreen.ScreenData
import Data.Maybe
import PrestoDOM
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Services.API
import Common.Types.App as CTA
import Components.SourceToDestination
import Helpers.Utils
import Data.String (split, Pattern(..))
import Data.Array (mapWithIndex , (!!), length, null,any)
import Components.DropDownCard as DropDownCard
import Effect (Effect)
import Language.Strings (getString)
import Language.Types as LType
import Data.Int as INT
import Screens.Types
import Data.Int(fromString)
import Components.PopUpModal as PopUpModal
import Font.Style (Style(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Debug(spy)



dummyLocation :: LocationInformation
dummyLocation = LocationInformation {
  address : {
    area : Nothing,
    areaCode : Nothing,
    building : Nothing,
    city : Nothing,
    country : Nothing,
    state : Nothing,
    street : Nothing,
    door : Nothing,  
    ward : Nothing,
    placeId : Nothing
  },
  placeId  : "",
  fullAddress : ""
}


sourceToDestinationConfig :: BookingAPIEntity -> SourceToDestination.Config
sourceToDestinationConfig (BookingAPIEntity entity) =
  let
    config = SourceToDestination.config
    (CTA.TripCategory tripCategory) = entity.tripCategory
    (LocationInformation source) = entity.fromLocation
    sourceFullAddress = source.fullAddress
    sourceArea = fromMaybe "" source.address.area
    sourceCity = fromMaybe "" source.address.city
    (LocationInformation destination) = fromMaybe dummyLocation entity.toLocation
    destinationFullAddress = destination.fullAddress
    destinationArea = fromMaybe "" destination.address.area
    destinationCity = fromMaybe "" destination.address.city
    sourceToDestinationConfig' =
      config
        { 
          id = Just "rideScreenPickupMap",
          separatorMargin = 0,
          showDestination = tripCategory.tag /= CTA.Rental,
          sourceImageConfig
        { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        , margin = MarginTop 3
        , width = V 18
        , height = V 18
        }
      , sourceTextConfig
        { text = if tripCategory.tag == CTA.InterCity then sourceCity else sourceArea
        , padding = Padding 2 0 2 2
        , margin = MarginHorizontal 12 15
        , color = Color.black900
        , ellipsize = true
        , maxLines = 1
        , textStyle = FontStyle.SubHeading2
        }
      , rideStartedAtConfig
        { text = sourceFullAddress
        , color = Color.black600
        , visibility = VISIBLE
        , padding = Padding 2 0 2 2
        , margin = MarginHorizontal 12 17
        , maxLines = if tripCategory.tag == CTA.InterCity then 1 else 2
        , ellipsize = true
        , textStyle = FontStyle.ParagraphText
        }
      , rideEndedAtConfig
        { text = destinationFullAddress
        , color = Color.black600
        , visibility = VISIBLE
        , padding = Padding 2 0 2 2
        , margin = MarginHorizontal 12 15
        , maxLines = 1
        , ellipsize = true
        , textStyle = FontStyle.ParagraphText
        }
      , destinationImageConfig
        { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_destination"
        , margin = MarginTop 0
        , width = V 20
        , height = V 23
        }
      , destinationTextConfig
        { text = if tripCategory.tag == CTA.InterCity then destinationCity else destinationArea
        , padding = Padding 2 0 2 2
        , margin = MarginHorizontal 12 15
        , color = Color.black900
        , ellipsize = true
        , maxLines = 1
        , textStyle = FontStyle.SubHeading2
        }
      , horizontalSeperatorConfig
        { visibility = GONE
        , background = Color.grey900
        , padding = Padding 2 0 2 2
        , margin = Margin 12 12 15 0
        }
      , pillsConfig
        { visibility = VISIBLE
        }
      , overrideSeparatorCount = if sourceFullAddress == "" then 4 else 6
      }
  in
    sourceToDestinationConfig'

      
rideSummaryCardConfig :: BookingAPIEntity -> RideSummaryCard.Config
rideSummaryCardConfig (BookingAPIEntity entity) = 
  let 
    vehicleServiceTier = entity.vehicleServiceTier
    currency = entity.currency
    estimatedFare = entity.estimatedFare
    isAirConditioned = (fromMaybe false entity.isAirConditioned) || (fromMaybe false (map (\x -> x > 0.0) entity.vehicleServiceTierAirConditioned)) 
    isVehicleAirConditioned = any (_ == vehicleServiceTier)["COMFY","ECO","SUV","HATCHBACK_TIER","TAXI_PLUS","SUV_PLUS", "HERITAGE_CAB"]
    isVehicleNonAirConditioned = any (_ == vehicleServiceTier)["AUTO_RICKSHAW","SEDAN_TIER","PREMIUM","TAXI", "EV_AUTO_RICKSHAW"]
    vehicleServiceTierAirConditioned = isAirConditioned || (isVehicleAirConditioned && not isVehicleNonAirConditioned)
    vehicleServiceTierSeatingCapacity = fromMaybe 4 entity.vehicleServiceTierSeatingCapacity
    vehicleServiceTierName =  case vehicleServiceTier of 
                              "TAXI" -> "Mini"
                              _ -> entity.vehicleServiceTierName
    vehicleServiceTierAirConditionedText = if vehicleServiceTierAirConditioned then (getString LType.AC) else (getString LType.NON_AC)
    startTime = entity.startTime
    roundTrip = fromMaybe false entity.roundTrip
    returnTime = fromMaybe "" entity.returnTime
    (CTA.TripCategory tripCategory) = entity.tripCategory
  in 
      { vehicleServiceTierImageUrl : fetchImage FF_COMMON_ASSET (case vehicleServiceTier of
                                                                  "AUTO_RICKSHAW" -> "ic_auto_rickshaw"
                                                                  "SEDAN_TIER" -> "ny_ic_sedan"
                                                                  "COMFY" -> "ny_ic_sedan_ac"
                                                                  "ECO" -> "ic_hatchback_ac"
                                                                  "PREMIUM" -> "ny_ic_sedan"
                                                                  "SUV" -> "ic_suv_ac"
                                                                  "HATCHBACK_TIER" -> "ic_hatchback_ac"
                                                                  "TAXI" -> "ic_taxi"
                                                                  "TAXI_PLUS" -> "ny_ic_sedan_ac"
                                                                  "SUV_PLUS" -> "ny_ic_suv_plus_side"
                                                                  "EV_AUTO_RICKSHAW" -> "ic_auto_rickshaw"
                                                                  "HERITAGE_CAB" -> "ny_ic_heritage_cab_side"
                                                                  _ -> "ny_ic_sedan")
      , rideAmount : (estimatedFare)
      , vehicleInfo :
        { vehicleServiceTierAirConditioned : vehicleServiceTierAirConditioned
        , vehicleServiceTierSeatingCapacity : vehicleServiceTierSeatingCapacity
        , vehicleServiceTierName : vehicleServiceTierName
        , airConditionedText : vehicleServiceTierAirConditionedText
        }
      , scheduleInfo :
        { pickUpTime : startTime
        , pickUpText : getString LType.PICKUP
        , dropText : getString LType.DROP
        , dropTime : returnTime
        , showDropTime : roundTrip
        }
      , rideTypePill : 
        { pillText : case tripCategory.tag of
                        CTA.InterCity -> if roundTrip then "Intercity Return" else "Intercity"
                        CTA.Rental -> "Rental"
                        CTA.OneWay -> "Regular"
                        _ -> "Regular"
        , pillImage : fetchImage FF_COMMON_ASSET (case tripCategory.tag of
                        CTA.InterCity -> "ny_ic_intercity_white"
                        CTA.Rental -> "ny_ic_rental_pill"
                        CTA.OneWay -> "ny_ic_regular_pill"
                        _ -> "ny_ic_intercity_white")
        , background : case tripCategory.tag of
                        CTA.InterCity -> Color.blue800
                        CTA.Rental -> Color.blueGreen
                        CTA.OneWay -> Color.green900
                        _ -> Color.blue600
        }
      }

dropDownCard :: Boolean -> String -> (forall w . PrestoDOM (Effect Unit) w) -> DropDownCard.Config 
dropDownCard isOpen title layout = 
  { isOpen : isOpen
  , title : title
  , layout : layout
  , openArrowImage : fetchImage FF_COMMON_ASSET "ny_ic_arrow_up"
  , closeArrowImage : fetchImage FF_COMMON_ASSET "ny_ic_arrow_down"
  , id :""
  , titleBackground : Color.white900
  , cardMargin : Margin 16 0 16 16
  , cardPadding : Padding 16 16 16 16
  , headingPadding : Padding 2 2 2 2
  , imageHeight : V 26
  , imageWidth : V 26
  , headingCornerRadius : 0.0
  }



formatSecIntoHourMins :: Int -> String
formatSecIntoHourMins seconds =
  let 
    hours = seconds `div` 3600
    mins = (seconds `mod` 3600) `div` 60
  in (if hours > 0 then show hours <> " hr " else "") <> show mins <> " min"

cancelScheduledRideConfig :: RideSummaryScreenState -> PopUpModal.Config
cancelScheduledRideConfig state = PopUpModal.config
        { optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 24 0 24 20
        , gravity = CENTER
        , margin = MarginHorizontal 20 20
        , primaryText
          { text = getString LType.CANCEL_SCHEDULED_RIDE
          , margin = Margin 16 16 16 10
          , textStyle = Heading2
          }
        , secondaryText
          { text = getString LType.CANCEL_SCHEDULED_RIDE_DESC
          , margin = MarginHorizontal 16 16
          }
        , option1
          { text = getString LType.CONFIRM_CANCELLATION
          , color = Color.yellow900
          , background = Color.black900
          , width = MATCH_PARENT
          , margin = MarginVertical 20 10
          , enableRipple = true
          }
        , option2
          { text = getString LType.DISMISS
          , color = Color.black900
          , background = Color.white900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , enableRipple = true
          , margin = MarginBottom 0
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { visibility = GONE
          }
        , backgroundClickable = false
        }

  
