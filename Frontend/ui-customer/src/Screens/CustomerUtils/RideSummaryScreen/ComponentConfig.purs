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
import Data.Array (mapWithIndex , (!!), length, null)
import Components.DropDownCard as DropDownCard
import Effect (Effect)
import Language.Strings (getString)
import Language.Types as LType
import Data.Int as INT
import Screens.Types
import Data.Int(fromString)



dummyLocation :: LocationInformation
dummyLocation = LocationInformation {
  address : {
    area : Just "",
    areaCode : Just "",
    building : Just "",
    city : Just "",
    country :Just "",
    state : Just "",
    street : Just "",
    door : Just "",  
    ward : Just "",
    placeId : Just ""
  },
  createdAt : "",
  id  : "",
  lat : 0.0,
  lon : 0.0,
  updatedAt :"",
  fullAddress : ""
}


sourceToDestinationConfig :: BookingAPIEntity -> SourceToDestination.Config
sourceToDestinationConfig (BookingAPIEntity entity) =
  let
    config = SourceToDestination.config
    (CTA.TripCategory tripCategory) = entity.tripCategory
    (LocationInformation source) = entity.fromLocation
    -- (Address sourceAddress) = source.address
    sourceFullAddress = source.fullAddress
    sourceArea = fromMaybe "" source.address.area
    sourceCity = fromMaybe "" source.address.city
    (LocationInformation destination) = fromMaybe dummyLocation entity.toLocation
    -- (Address destinationAddress) = destination.address
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
        , margin = MarginHorizontal 12 15
        , maxLines = 2
        , ellipsize = true
        , textStyle = FontStyle.ParagraphText
        }
      , rideEndedAtConfig
        { text = destinationFullAddress
        , color = Color.black600
        , visibility = VISIBLE
        , padding = Padding 2 0 2 2
        , margin = MarginHorizontal 12 15
        , maxLines = 2
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
        { visibility = GONE,
          pillList = makePillList (BookingAPIEntity entity) entity.tripCategory
        }
      }
  in
    sourceToDestinationConfig'


makePillList :: BookingAPIEntity -> CTA.TripCategory -> Array PillInfo
makePillList (BookingAPIEntity entity) (CTA.TripCategory tripCategory) =
  let
    estimatedDuration = formatSecIntoHourMins (fromMaybe 0 entity.estimatedDuration)
    estimatedDistance = show ((fromMaybe 0 entity.estimatedDistance) / 1000)
  in
    case tripCategory.tag of
      CTA.InterCity -> [  
                          { text : estimatedDistance <> " km"
                          , imageUrl : ""
                          , imageVisibility : GONE
                          },
                          { text : estimatedDuration
                          , imageUrl : ""
                          , imageVisibility : GONE
                          },
                          { text : "Round Trip" -- i dont think we gonna use this pillList
                          , imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_roundtrip_indicator"
                          , imageVisibility : VISIBLE
                          }
                        ]
      CTA.Rental -> [  
                      { text : estimatedDistance <> " km"
                      , imageUrl : ""
                      , imageVisibility : GONE
                      },
                      { text : estimatedDuration
                      , imageUrl : ""
                      , imageVisibility : GONE
                      }
                    ]
      CTA.OneWay -> [  
                      { text : estimatedDistance <> " km"
                      , imageUrl : ""
                      , imageVisibility : GONE
                      }
                    ]
      _ -> []
      
rideSummaryCardConfig :: BookingAPIEntity -> RideSummaryCard.Config
rideSummaryCardConfig (BookingAPIEntity entity) = 
  let 
    vehicleServiceTier = entity.vehicleServiceTier
    currency = entity.currency
    estimatedFare = entity.estimatedFare
    vehicleServiceTierAirConditioned =(case vehicleServiceTier of --TODO :  need this to be configured from backend 
                                              "AUTO_RICKSHAW" -> false
                                              "SEDAN_TIER" -> false
                                              "COMFY" -> true
                                              "ECO" -> true
                                              "PREMIUM" -> false
                                              "SUV" -> true
                                              "HATCHBACK_TIER" -> true
                                              "TAXI" -> false
                                              "TAXI_PLUS" -> true
                                              _ -> false)
    --fromMaybe false (map (\x -> x >= 0.0) entity.vehicleServiceTierAirConditioned)
    vehicleServiceTierSeatingCapacity = (fromMaybe 0 (fromString $ fromMaybe "0" entity.vehicleServiceTierSeatingCapacity))
    vehicleServiceTierName =  case vehicleServiceTier of 
                              "TAXI" -> "Mini"
                              _ -> entity.vehicleServiceTierName
    startTime = entity.startTime
    roundTrip = fromMaybe false entity.roundTrip
    returnTime = fromMaybe "" entity.returnTime
    (CTA.TripCategory tripCategory) = entity.tripCategory
  in 
      { vehicleServiceTierImageUrl : fetchImage FF_COMMON_ASSET (case vehicleServiceTier of
                                                                  "AUTO_RICKSHAW" -> "ny_ic_auto_side_view"
                                                                  "SEDAN_TIER" -> "ny_ic_sedan"
                                                                  "COMFY" -> "ny_ic_sedan_ac"
                                                                  "ECO" -> "ic_hatchback_ac"
                                                                  "PREMIUM" -> "ny_ic_sedan"
                                                                  "SUV" -> "ic_suv_ac"
                                                                  "HATCHBACK_TIER" -> "ic_hatchback_ac"
                                                                  "TAXI" -> "ic_taxi"
                                                                  "TAXI_PLUS" -> "ny_ic_sedan_ac"
                                                                  _ -> "ny_ic_sedan")
      , rideAmount : (estimatedFare)
      , vehicleInfo :
        { vehicleServiceTierAirConditioned : vehicleServiceTierAirConditioned
        , vehicleServiceTierSeatingCapacity : vehicleServiceTierSeatingCapacity
        , vehicleServiceTierName : vehicleServiceTierName
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
  }



formatSecIntoHourMins :: Int -> String
formatSecIntoHourMins seconds =
  let 
    hours = seconds `div` 3600
    mins = (seconds `mod` 3600) `div` 60
  in (if hours > 0 then show hours <> " hr " else "") <> show mins <> " min"
