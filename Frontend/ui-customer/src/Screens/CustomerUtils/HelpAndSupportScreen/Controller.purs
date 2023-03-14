module Screens.HelpAndSupportScreen.Controller where

import Accessor (_driverRatings, _contents, _toLocation, _amount, _driverName, _list, _vehicleNumber, _id, _computedPrice, _shortRideId, _rideRating)
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard as IndividualRideCard
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array ((!!), null, filter)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (convertUTCtoISC)
import JBridge (showDialer)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, bind, discard, show, unit, map, ($), (<>), (==), void)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (HelpAndSupportScreenState)
import Services.API (RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..))
import Services.Config (getSupportNumber)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
      BackPressed flag -> do
        trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
        if flag then trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "backpress_in_call_confirmation"
        else trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      GenericHeaderActionController act -> case act of
        GenericHeader.PrefixImgOnClick -> do
          trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "back_icon"
          trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "forward_icon"
      ContactUs -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "contact_us"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      ViewRides -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_rides"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      ReportIssue -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "report_issue"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      NoRidesActionController act -> case act of
        ErrorModal.PrimaryButtonActionController act -> case act of
          PrimaryButton.OnClick -> do
            trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "no_rides_error_modal_action" "primary_button_go_home"
            trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
          PrimaryButton.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "error_modal_action" "primary_button_no_action"
      APIFailureActionController act -> case act of
        ErrorModal.PrimaryButtonActionController act -> case act of
          PrimaryButton.OnClick -> do
            trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "api_failure_error_modal_action" "primary_button_go_back"
            trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
          PrimaryButton.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "api_failure_error_modal_action" "primary_button_no_action"
      PopupModelActionController act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "call_driver_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "call_driver_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "countdown_updated"
      SourceToDestinationActionController (SourceToDestination.Dummy) -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "source_to_destination_updated"
      FAQs -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "faq_action"
      RideBookingListAPIResponseAction rideList status -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "ride_booking_list_api_response"
      CallSupport -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "call_support"


data Action = BackPressed Boolean
            | SourceToDestinationActionController SourceToDestination.Action 
            | GenericHeaderActionController GenericHeader.Action
            | ContactUs 
            | FAQs 
            | ViewRides 
            | ReportIssue
            | RideBookingListAPIResponseAction RideBookingListRes String
            | NoRidesActionController ErrorModal.Action
            | APIFailureActionController ErrorModal.Action
            | PopupModelActionController PopUpModal.Action
            | AfterRender
            | CallSupport

data ScreenOutput = GoBack 
                  | GoToSupportScreen String
                  | GoToTripDetails HelpAndSupportScreenState 
                  | GoToMyRides
                  | GoHome
                  | UpdateState HelpAndSupportScreenState

eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState

eval (BackPressed flag ) state = if state.props.isCallConfirmation 
  then continue state{props{isCallConfirmation = false}}
  else exit GoBack

eval ContactUs state = exit $ GoToSupportScreen state.data.bookingId

eval ReportIssue state = exit $ GoToTripDetails state

eval CallSupport state = continue state{props{isCallConfirmation = true}}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure $ BackPressed state.props.isCallConfirmation]

eval ViewRides state = exit $ GoToMyRides

eval (RideBookingListAPIResponseAction rideList status) state = do 
  case status of 
      "success" -> do 
                    if (null (rideList^._list)) then continue state {data{isNull = true}, props{apiFailure = false}}
                      else do 
                        let list = myRideListTransform (rideList ^._list)
                        case (null (list)) of
                          true -> continue state {data{isNull = true}, props{apiFailure=false}}
                          _    -> do  
                                    let newState = (myRideListTransform (rideList ^._list))!!0
                                    updateAndExit (fromMaybe dummyState newState) (UpdateState (fromMaybe dummyState newState))
      "failure"   -> continue state{props{apiFailure = true}}
      _           -> continue state

eval (PopupModelActionController (PopUpModal.OnButton1Click)) state = continue state{props{isCallConfirmation = false}}

eval (PopupModelActionController (PopUpModal.OnButton2Click)) state = do
  void $ pure $ showDialer $ getSupportNumber ""
  continue state{props{isCallConfirmation = false}}

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit GoBack

eval (NoRidesActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit GoHome

eval _ state = continue state

myRideListTransform :: Array RideBookingRes -> Array HelpAndSupportScreenState
myRideListTransform listRes = filter (\item -> (item.data.status == "COMPLETED")) (map(\(RideBookingRes ride) -> {
    data:{
        date : (convertUTCtoISC (ride.createdAt) "DD/MM/YYYY"),
        time : (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
        source: decodeAddress (Booking ride.fromLocation),
        destination: (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
        rating: (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _rideRating)),
        driverName :((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _driverName) ,
        totalAmount : ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
        status : (ride.status),
        isNull : false,
        rideStartTime : (convertUTCtoISC (fromMaybe "" ride.rideStartTime )"h:mm A"),
        rideEndTime : (convertUTCtoISC (fromMaybe "" ride.rideEndTime )"h:mm A"),
        vehicleNumber : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleNumber),
        rideId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._id),
        tripId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._shortRideId),
        bookingId : ride.id

        },
    props : {
      apiFailure : false
    , isCallConfirmation : false
    }
    }) listRes)


dummyCard :: RideBookingRes
dummyCard =RideBookingRes {
  agencyNumber: "",
      status: "",
      fareBreakup: [dummyFareBreakUp],
      createdAt: "",
      discount: Nothing,
      estimatedTotalFare: 0,
      agencyName: "",
      rideList: [ dummyRideAPIEntity ],
      estimatedFare: 0,
      tripTerms: [
        ""
      ],
      id: "",
      updatedAt: "",
      rideStartTime : Nothing,
      rideEndTime : Nothing,
      duration : Nothing,
      bookingDetails: RideBookingAPIDetails{
        contents: RideBookingDetails {
          toLocation: BookingLocationAPIEntity {
            area: Just "",
            state: Just "",
            country: Just "",
            building:Just "",
            door: Just "",
            street:Just "",
            lat: 0.0,
            city: Just "",
            areaCode: Just "",
            lon: 0.0,
            ward : Nothing,
            placeId : Nothing
          }
        , estimatedDistance : Nothing
        },
        fareProductType: ""
      },
      fromLocation: BookingLocationAPIEntity {
        area:  Just "",
        state: Just "",
        country:Just  "",
        building:  Just "",
        door: Just "",
        street: Just "",
        lat: 0.0,
        city: Just "",
        areaCode: Just "",
        lon: 0.0,
        ward : Nothing,
        placeId : Nothing
      }
    }

dummyState :: HelpAndSupportScreenState
dummyState = {
  data : {
    date : "",
    time : "",
    source : "",
    destination : "",
    rating : 0,
    driverName : "",
    totalAmount : "",
    status : "",
    isNull : true,
    rideStartTime : "",
    rideEndTime : "",
    vehicleNumber:"",
    rideId : "",
    tripId : "",
    bookingId : ""
  },
  props:{
    apiFailure : false
  , isCallConfirmation : false
  }
}


dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0.0,description: ""}
