module Screens.RideSummaryScreen.Controller where

import ConfigProvider
import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Screens.Types
import Services.API as API

import Common.Types.App as CTA
import Components.DropDownCard.Controller as DropDownCardController
import Components.PopUpModal.Controller as PopUpModalController
import Components.RideSummaryCard as RideSummaryCard
import Components.SourceToDestination.Controller as SourceToDestinationController
import Data.Array (head)
import Data.Int as INT
import Data.Maybe (Maybe(..), fromMaybe , isJust)
import Data.String as DS
import Engineering.Helpers.Commons (getCurrentUTC)
import JBridge (showDialer)
import Language.Types (STR(..))
import PrestoDOM (Eval, continue, exit, update)
import PrestoDOM (updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (getFareProductType)
import Engineering.Helpers.Commons (compareUTCDate)
import Helpers.Utils (parseFloat,fetchAddressDetails)

instance showAction  ::  Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog



data Action = BackPressed String
              | AcceptClick
              | NoAction
              | SourceToDestinationActionController SourceToDestinationController.Action
              | RideSummaryCardActionController RideSummaryCard.Action
              | PickUp DropDownCardController.Action 
              | IncludedCharges DropDownCardController.Action 
              | ExcludedCharges DropDownCardController.Action 
              | Terms DropDownCardController.Action
              | RideConfirmed 
              | NotificationListener String NotificationBody
              | CancelRideActionController PopUpModalController.Action
              | CancelRide
              | CallDriver
              | ShowCallDialer CallType
              | CloseShowCallDialer
              | CheckFlowStatusAction
              | GetBooking API.RideBookingRes String
              | RefreshScreen

data ScreenOutput = GoBack 
                    | AcceptScheduleRide String String
                    | GoToHomeScreen String (Maybe String) String
                    | CancelScheduledRide String String
                    | NotificationListenerSO String NotificationBody
                    | CallDriverSO RideSummaryScreenState CallType String
                    | RefreshScreenSO (Maybe String)
eval :: Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval (BackPressed startTimeUTC) state = if state.props.isBookingAccepted then exit (GoToHomeScreen startTimeUTC state.data.bookingId state.data.fromScreen) else exit $ GoBack
                                
eval AcceptClick state = do
                          let (BookingAPIEntity entity) = state.data.rideDetails
                              quoteId = entity.id
                              startTimeUTC = entity.startTime
                          exit $ AcceptScheduleRide quoteId startTimeUTC
eval RideConfirmed state = do
                      let (BookingAPIEntity entity) = state.data.rideDetails 
                          startTimeUTC = entity.startTime

                      exit $ GoToHomeScreen startTimeUTC state.data.bookingId state.data.fromScreen
eval (Terms (DropDownCardController.OnClick config)) state = do
                                                      continue state{ props{termsAndConditionOpen = not state.props.termsAndConditionOpen}}
eval (ExcludedCharges (DropDownCardController.OnClick config)) state = do
                                                                continue state{ props{excludedChargesOpen = not state.props.excludedChargesOpen}}
eval (IncludedCharges (DropDownCardController.OnClick config)) state = do
                                                                continue state{ props{includedChargesOpen = not state.props.includedChargesOpen}}
eval (PickUp (DropDownCardController.OnClick config)) state = do
                                                      continue state{ props{pickUpOpen = not state.props.pickUpOpen}}
eval (NotificationListener notificationType notificationBody) state = exit $ NotificationListenerSO notificationType notificationBody 


eval CancelRide state = continue state{ props{isCancelRide = true}}

eval (CancelRideActionController (PopUpModalController.OnButton1Click)) state = do
          let bookingId = fromMaybe "" state.data.bookingId
              fromScreen = state.data.fromScreen
          exit $ CancelScheduledRide bookingId fromScreen

eval (CancelRideActionController (PopUpModalController.OnButton2Click)) state = continue state{props{isCancelRide = false}}

eval CallDriver  state = continue state{props{showCallPopUp = true}}

eval CloseShowCallDialer  state = continue state { props { showCallPopUp = false } }


eval (ShowCallDialer item) state = do
  case item of
    ANONYMOUS_CALLER -> callDriver state "ANONYMOUS"
    DIRECT_CALLER -> callDriver state "DIRECT"


eval (GetBooking apiResp status) state = do 
  case status of 
    "success" -> do 
                  let
                      (API.RideBookingRes resp) = apiResp
                  continue state {data{
                          rideDetails = fetchRideDetails state apiResp,
                          rideList = resp.rideList,
                          merchantExoPhone = resp.merchantExoPhone,
                          bookingId = Just resp.id
                        }
                        ,props{
                          isBookingAccepted = true,
                          pickUpOpen = true,
                          shimmerVisibility = false
                        }}
    "failure" -> continue state{props{hasApiFailed = true}}
    _ -> continue state

eval RefreshScreen state = do exit $ RefreshScreenSO state.data.bookingId

eval CheckFlowStatusAction state = update state

eval _ state = continue state


callDriver :: RideSummaryScreenState -> String -> Eval Action ScreenOutput RideSummaryScreenState
callDriver state callType = do
  let newState = state{props{ showCallPopUp = false }}
      (API.RideAPIEntity driverInfo) = fromMaybe dummyRideAPIEntity (head $ state.data.rideList)
      driverNumber = case callType of
                        "DIRECT" -> (fromMaybe state.data.merchantExoPhone driverInfo.driverNumber)
                        _ -> if (DS.take 1 state.data.merchantExoPhone) == "0" then state.data.merchantExoPhone else "0" <> state.data.merchantExoPhone
  updateWithCmdAndExit newState
    [ do
        _ <- pure $ showDialer driverNumber false
        pure NoAction
    ] $ CallDriverSO newState (if callType == "DIRECT" then DIRECT_CALLER else ANONYMOUS_CALLER) driverNumber


fetchRideDetails :: RideSummaryScreenState -> API.RideBookingRes -> BookingAPIEntity 
fetchRideDetails state apiResp =
  let
    (API.RideBookingRes resp) = apiResp
    (API.RideBookingAPIDetails bookingDetails) = resp.bookingDetails
    (API.RideBookingDetails contents) = bookingDetails.contents
    fareProductType = getFareProductType (bookingDetails.fareProductType)
    (API.BookingLocationAPIEntity stopLocation) = fromMaybe dummyBookingDetails (case fareProductType of 
                                                                                    RENTAL -> contents.stopLocation
                                                                                    INTER_CITY -> contents.toLocation
                                                                                    _ -> contents.toLocation)
        
    (API.BookingLocationAPIEntity fromLocation) = resp.fromLocation
    startTimeUTC = fromMaybe (getCurrentUTC "") resp.rideScheduledTime
    roundTrip = isJust resp.returnTime
    returnTimeUTC = fromMaybe (getCurrentUTC "") resp.returnTime
    estimatedDuration = if roundTrip then (Just (compareUTCDate returnTimeUTC startTimeUTC)) else resp.estimatedDuration
    estimatedDistance = fromMaybe 0 resp.estimatedDistance
    rideDetails =  BookingAPIEntity{
      currency : INR,
      estimatedDistance : Just $ formatDistance roundTrip estimatedDistance, 
      estimatedDuration : estimatedDuration, 
      estimatedFare : (getCurrency appConfig) <> show resp.estimatedFare, 
      fromLocation :(LocationInformation{
              address : {
                    area : fromLocation.area,
                    areaCode : fromLocation.areaCode,
                    building :fromLocation.building,
                    city :fromLocation.city,
                    country : fromLocation.country,
                    state : fromLocation.state,
                    street : fromLocation.street,
                    door : fromLocation.door,
                    ward :fromLocation.ward,
                    placeId : fromLocation.placeId
              },
              placeId  : fromMaybe "" fromLocation.placeId,
              fullAddress : fetchAddressDetails $ resp.fromLocation
            }),
      id : resp.id,
      isScheduled : resp.isScheduled,
      returnTime : resp.returnTime,
      roundTrip : Just roundTrip, 
      isAirConditioned: resp.isAirConditioned,
      startTime : startTimeUTC,
      toLocation : Just (LocationInformation{
              address : {
                    area : stopLocation.area,
                    areaCode : stopLocation.areaCode,
                    building :stopLocation.building,
                    city :stopLocation.city,
                    country : stopLocation.country,
                    state : stopLocation.state,
                    street : stopLocation.street,
                    door : stopLocation.door,
                    ward :stopLocation.ward,
                    placeId : stopLocation.placeId
              },
              placeId  : fromMaybe "" stopLocation.placeId,
              fullAddress : fetchAddressDetails $ (API.BookingLocationAPIEntity stopLocation) 
             }),
      tripCategory : CTA.TripCategory {
        contents : Just "",
        tag : getTag fareProductType 
      },
      vehicleServiceTier : fromMaybe "" resp.vehicleServiceTierType,
      vehicleServiceTierAirConditioned : resp.vehicleServiceTierAirConditioned,
      vehicleServiceTierName : fromMaybe "" resp.serviceTierName,
      vehicleServiceTierSeatingCapacity : Just $ fromMaybe 4 resp.vehicleServiceTierSeatingCapacity
    }
  in rideDetails
  where 
  formatDistance roundTrip duration =
    let 
      mul = 1.0
      durationDecimals = INT.toNumber duration
      duration' = if mul*durationDecimals < 1000.0 then (parseFloat (mul*durationDecimals) 0) <> " m" else (parseFloat ((mul*durationDecimals)/1000.0) 0) <> " km" 
    in  
      duration'



getTag :: FareProductType -> CTA.TripCategoryTag
getTag fpt = 
  case fpt of 
    INTER_CITY -> CTA.InterCity
    RENTAL -> CTA.Rental
    _ -> CTA.OneWay
