module Screens.RideSummaryScreen.ComponentConfig where

import Prelude

import Components.SourceToDestination as SourceToDestination
import Components.RideSummaryCard as RideSummaryCard
import Screens.RideSummaryScreen.ScreenData
import Data.Maybe
import PrestoDOM
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Services.API
import Common.Types.App as CTA
import Components.SourceToDestination
import Resource.Constants
import Helpers.Utils
import Data.String (split, Pattern(..))
import Data.Array (mapWithIndex , (!!), length, null)
import Components.DropDownCard as DropDownCard
import Effect (Effect)
import Language.Strings (getString)
import Language.Types as LType
import Data.Int as INT
import Components.PopUpModal as PopUpModal
import Screens.Types as ST
import Language.Strings (getString)
import Language.Types (STR(..)) as LT
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Services.API (ServiceTierType(..))
import Debug
import Mobility.Prelude (boolToVisibility)
import Resource.Localizable.StringsV2 (getStringV2)
import Resource.Localizable.TypesV2



dummyLocation :: Location
dummyLocation = Location {
  address : LocationAddress{
    area : Nothing,
    areaCode : Nothing,
    building : Nothing,
    city : Nothing,
    country :Nothing,
    state : Nothing,
    street : Nothing,
    door : Nothing,
    fullAddress : Nothing
  },
  createdAt : "",
  id  : "",
  lat : 0.0,
  lon : 0.0,
  updatedAt :""
}


sourceToDestinationConfig :: BookingAPIEntity -> SourceToDestination.Config
sourceToDestinationConfig (BookingAPIEntity entity) =
  let
    config = SourceToDestination.config
    (CTA.TripCategory tripCategory) = entity.tripCategory
    (Location source) = entity.fromLocation
    (LocationAddress sourceAddress) = source.address
    sourceFullAddress = fromMaybe "" sourceAddress.fullAddress
    sourceArea =  fromMaybe "" sourceAddress.area
    sourceCity = fromMaybe "" sourceAddress.city
    (Location destination) = fromMaybe dummyLocation entity.toLocation
    (LocationAddress destinationAddress) = destination.address
    destinationFullAddress = fromMaybe "" destinationAddress.fullAddress
    destinationArea = fromMaybe "" destinationAddress.area
    destinationCity = fromMaybe "" destinationAddress.city
    sourceToDestinationConfig' =
      config
        {
          id = Just "rideScreenPickupMap",
          separatorMargin = 0,
          showDestination =  tripCategory.tag /= CTA.Rental,
          sourceImageConfig
        { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
        , margin = MarginTop 3
        , width = V 18
        , height = V 18
        }
      , sourceTextConfig
        { text =  if tripCategory.tag == CTA.InterCity then sourceCity else sourceArea
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
        { text =   destinationFullAddress
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
        { visibility = VISIBLE,
          pillList = []
        }
      , overrideSeparatorCount = 15
      }
  in
    sourceToDestinationConfig'


makePillList :: BookingAPIEntity -> CTA.TripCategory -> Array PillInfo
makePillList (BookingAPIEntity entity) (CTA.TripCategory tripCategory) =
  let
    estimatedDuration = formatSecIntoHourMins (fromMaybe 0 entity.estimatedDuration)
    estimatedDistance = show ((fromMaybe 0 entity.estimatedDistance) / 1000)
    roundTrip = fromMaybe false entity.roundTrip
    tripType  = if roundTrip then LType.ROUND_TRIP else LType.INTERCITY

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
                          { text : getString tripType
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
    vehicleServiceTierAirConditioned = fromMaybe false entity.isAirConditioned
    vehicleServiceTierSeatingCapacity = (fromMaybe 0 entity.vehicleServiceTierSeatingCapacity)
    vehicleServiceTierName =  entity.vehicleServiceTierName
    startTime = entity.startTime
    (Location destination) = fromMaybe dummyLocation entity.toLocation
    (LocationAddress destinationAddress) = destination.address
    destinationFullAddress = fromMaybe "" destinationAddress.fullAddress
    roundTrip = fromMaybe false entity.roundTrip
    vehicleServiceTierAirConditionedText = if vehicleServiceTierAirConditioned then (getString LType.AC) else (getString LType.NON_AC)
    pickupFormattedTime' = rentalPickupTimeFormat entity.startTime

    returnTime = fromMaybe "" entity.returnTime
    (CTA.TripCategory tripCategory) = entity.tripCategory
  in
      { vehicleServiceTierImageUrl : fetchImage FF_COMMON_ASSET $ getVehicleServiceTierImage vehicleServiceTier
      , rideAmount : (case currency of
                      INR -> "₹"
                      USD -> "$"
                      EUR -> "€") <> (show  (INT.ceil estimatedFare))
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
        , estimatedDistance : maybe "" (\estimatedDistance ->  (show (estimatedDistance / 1000)) <> " km") entity.estimatedDistance
        , estimatedDuration : maybe "" (\estimatedDuration ->  (show (estimatedDuration / 3600)) <> "hr") entity.estimatedDuration
        , pickupFormattedTime : pickupFormattedTime'
        }
      , rideTypePill :{
        pillText :
          case tripCategory.tag of
            CTA.InterCity -> if roundTrip then getString LType.INTERCITY_RETURN else getString LType.INTERCITY
            CTA.Rental -> getString LType.RENTAL
            CTA.OneWay -> getString LType.REGULAR
            _ -> "Regular"
          <> " " <> (getStringV2 package)

        , pillImage : fetchImage FF_COMMON_ASSET (case tripCategory.tag of
                        CTA.InterCity -> "ny_ic_intercity"
                        CTA.Rental -> "ny_ic_rental_pill"
                        CTA.OneWay -> "ny_ic_regular_pill"
                        _ -> "ny_ic_intercity")
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
  , id : ""
  , titleBackground : ""
  , cardMargin : Margin 16 14 16 14
  , cardPadding : Padding 15 10 15 10
  , headingPadding : Padding 4 4 4 4
  , imageHeight : V 30
  , imageWidth : V 30
  , headingCornerRadius : 0.0
  }

cancelConfirmationConfig :: RideSummaryScreenState -> PopUpModal.Config
cancelConfirmationConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 24 16 20 ,
    primaryText {
      text =  getString LT.FREQUENT_CANCELLATIONS_WILL_LEAD_TO_BLOCKING
    , margin = Margin 16 24 16 0 },
    secondaryText {
      visibility = boolToVisibility $ state.props.showPopUp,
      text = "",
      margin = MarginTop 6
      },
    option1 {
      text = (getString LT.CONTINUE)
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , isClickable = state.data.cancelRidePopUpData.continueEnabled
    , timerValue = state.data.cancelRidePopUpData.delayInSeconds
    , enableTimer = true
    , timerID = "cancelConfirmationTimer"
    , background = Color.white900
    , strokeColor = Color.black500
    , color = Color.black700
    , enableRipple = state.data.cancelRidePopUpData.continueEnabled
    },
    option2 {
      text = (getString LT.GO_BACK)
    , margin = MarginLeft 12
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    , enableRipple = true
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_frequent_cancellation_blocking"
    , visibility = VISIBLE
    , margin = Margin 16 20 16 0
    , height = V 178
    },
    timerId = "PopUp"
  }
  in popUpConfig'


errorPopUpConfig :: RideSummaryScreenState -> PopUpModal.Config
errorPopUpConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 24 16 20 ,
    primaryText {
      text = getString LType.RIDE_ASSIGNED_TO_ANOTHER_DRIVER
    , margin = Margin 16 24 16 0 },
    secondaryText {
       text = getString LType.OR_RIDE_IS_CANCELLED_BY_CUSTOMER
      ,margin = Margin  6 4 0 0
      ,color  = Color.black900
      ,textStyle = FontStyle.SubHeading2
      },
    option1 {
      text = (getString LT.CONTINUE)
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , isClickable = true
    , background = Color.white900
    , strokeColor = Color.black500
    , color = Color.black700
    , enableRipple = state.data.cancelRidePopUpData.continueEnabled
    , visibility = false
    },
    option2 {
      text = (getString LT.GOT_IT)
    , margin = MarginLeft 12
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    , enableRipple = true
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_exclamation_mark"
    , visibility = VISIBLE
    , margin = Margin 16 20 16 0
    , height = V 50
    }
  }
  in popUpConfig'


getLanguageBasedRentalAudio :: String -> String
getLanguageBasedRentalAudio language =
  case language of
    "EN_US" -> fetchAudio "rental_ride_accepted" "en" -- TODO: Once other language audio ready change it.
    "KN_IN" -> fetchAudio "rental_ride_accepted" "en"
    "HI_IN" -> fetchAudio "rental_ride_accepted" "en"
    "TA_IN" -> fetchAudio "rental_ride_accepted" "en"
    "TE_IN" -> fetchAudio "rental_ride_accepted" "en"
    _ -> fetchAudio "rental_ride_accepted" "en"
