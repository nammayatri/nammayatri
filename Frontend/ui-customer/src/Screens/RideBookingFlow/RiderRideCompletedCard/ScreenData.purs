module Screens.RideBookingFlow.RiderRideCompletedCard.ScreenData where

import Screens.Types as ST
import Prelude
import PrestoDOM
import Data.Maybe
import Services.API (RideBookingRes(..), RideBookingAPIDetails(..), RideBookingDetails(..))
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import ConfigProvider
import Common.Types.App as CT
import Screens.Types (FareProductType(..), RentalRowConfig(..)) as FPT
import Language.Strings (getString)
import Language.Types
import Styles.Colors as Color
import Screens.Types (RatingCard(..))
import Components.MessagingView.Controller (ChatContacts, dummyChatRecipient)


initData :: ST.RiderRideCompletedScreenState
initData = let
  config = getAppConfig appConfig
  in
  {  topCard : {
      title : "",
      finalAmount : 0,
      initialAmount : 0,
      fareUpdatedVisiblity : false,
      infoPill : fareUpdatePill
    }
  , rideId : ""
  , favDriverInfoCard : false
  , isFreeRide : false 
  , accessibility : ENABLE
  , isRatingCard : false 
  , ratingCard : riderRatingCard
  , driverInfoCardState : dummyDriverInfo
  , needHelpText : getString NEED_HELP
  , recordedView : false
  , timerId : ""
  , timerValue : "0:00"
  , countDownValue : "0:00"
  , rentalRowDetails : dummyRentalRowConfig
  , rentalBookingData : dummyRentalBookingConfig
  , showRentalRideDetails : false
  , ratingViewState : {
      selectedYes : Nothing,
      selectedRating : -1,
      issueReportActiveIndex : Nothing ,
      issueReasonCode : Nothing,
      openReportIssue : false,
      doneButtonVisibility : false,
      issueReason : Nothing,
      issueDescription : "",
      rideBookingRes : dummyRideBooking,
      wasOfferedAssistance : Nothing,
      nightSafety : Nothing
    }
  , isKeyBoardOpen : false
  , rideRatingState : dummyPreviousRiderating
  , isSafetyCenterDisabled : false
  , bookingId : ""
  , config : config
  , rideDuration : Just 0
  , additionalCharges : []
  , showSafetyCenter : false
  , goToLastBanner : true
  , customerIssue : {
      currentPageIndex : 0
    , showIssueBanners : true
    , hasAccessibilityIssue : false
    , hasSafetyIssue : false 
    , demandExtraTollAmountIssue : false
    , bannerComputedView : Nothing   
    , customerResponse : [
      {
        issueType : CT.NightSafety
      , selectedYes : Nothing
      }
    , {
        issueType : CT.Accessibility
      , selectedYes : Nothing
      }
    , {
        issueType : CT.DemandExtraTollAmount
      , selectedYes : Just false
    }
    , {
        issueType : CT.NoIssue
      , selectedYes : Nothing
      }
    ]
    , respondedValidIssues : false
    , buttonActive : false
  }
}

dummyPreviousRiderating :: RatingCard
dummyPreviousRiderating = {
  rideId : ""
, rating : 0
, driverName : ""
, finalAmount : 0
, rideStartTime : ""
, rideEndTime : ""
, source : ""
, destination : ""
, rideStartDate : ""
, vehicleNumber : ""
, status : ""
, shortRideId : ""
, bookingId : ""
, rideEndTimeUTC : ""
, dateDDMMYY : ""
, offeredFare : 0
, distanceDifference : 0
, feedback : ""
, feedbackList : []
}

dummyRentalRowConfig :: FPT.RentalRowConfig
dummyRentalRowConfig = {
    rideTime : ""
  , rideDistance : ""
  , rideDistanceInfo : ""
  , rideStartedAt : ""
  , rideEndedAt : ""
  , estimatedFare : ""
  , extraTimeFare : ""
  , extraDistanceFare : ""
  , totalFare : ""
  , rideDetailsTitle : ""
  , fareUpdateTitle : ""
  , surcharges : ""
}

dummyDriverInfo :: ST.DriverInfoCard
dummyDriverInfo =
  { otp : ""
  , driverName : ""
  , eta : Nothing
  , vehicleDetails : ""
  , registrationNumber : ""
  , rating : 0.0
  , startedAt : ""
  , endedAt : ""
  , source : ""
  , destination : ""
  , rideId : ""
  , price : 0
  , sourceLat : 0.0
  , sourceLng : 0.0
  , initialPickupLat : 0.0
  , initialPickupLon : 0.0
  , editPickupAttemptsLeft : 0
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  , bppRideId : ""
  , driverNumber : Nothing
  , merchantExoPhone : ""
  , createdAt : ""
  , initDistance : Nothing
  , config : getAppConfig appConfig
  , vehicleVariant : ""
  , sourceAddress : dummyAddress
  , destinationAddress : dummyAddress
  , status : ""
  , serviceTierName : Nothing
  , vehicleModel : ""
  , vehicleColor : ""
  , providerName : ""
  , providerType : CT.ONUS
  , rentalData : dummyRentalBookingConfig
  , fareProductType : FPT.ONE_WAY
  , driversPreviousRideDropLocLat : Nothing
  , driversPreviousRideDropLocLon : Nothing
  , spLocationName : Nothing
  , addressWard : Nothing
  , hasToll : false
  , isAlreadyFav : false
  , favCount : 0
  , rideDuration : Just 0
  , currentChatRecipient : dummyChatRecipient
  , rideScheduledAtUTC : Nothing
  , destinationReached : false
  , destinationReachedAt : 0
  , senderDetails : Nothing
  , receiverDetails : Nothing
  , estimatedTimeToReachDestination : Nothing
  , isAirConditioned : Nothing
  }

dummyRentalBookingConfig :: CT.RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : ""
  , rideEndedAt : ""
  , extraDistanceFare : ""
  , extraTimeFare : ""
  }

dummyAddress :: ST.Address
dummyAddress = 
  { "area"      : Nothing
  , "state"     : Nothing
  , "country"   : Nothing
  , "building"  : Nothing
  , "door"      : Nothing
  , "street"    : Nothing
  , "city"      : Nothing
  , "areaCode"  : Nothing
  , "ward"      : Nothing
  , "placeId"   : Nothing
  }

dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : Nothing,
  status : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  duration : Nothing,
  fareBreakup :[],
  createdAt : "",
  discount : Nothing ,
  estimatedTotalFare : 0,
  agencyName : "",
  rideList :[] ,
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  hasNightIssue : Just true,
  updatedAt : "",
  bookingDetails : dummyRideBookingAPIDetails ,
  fromLocation :  dummyBookingDetails,
  initialPickupLocation : dummyBookingDetails,
  merchantExoPhone : "",
  specialLocationTag : Nothing,
  hasDisability : Nothing,
  sosStatus: Nothing,
  serviceTierName : Nothing,
  isValueAddNP : Nothing,
  providerName : Nothing,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  rideScheduledTime : Nothing,
  vehicleServiceTierType : Nothing,
  tollConfidence : Nothing,
  driversPreviousRideDropLocLat : Nothing,
  driversPreviousRideDropLocLon : Nothing,
  specialLocationName : Nothing,
  estimatedFareBreakup : [],
  isAlreadyFav : Nothing,
  favCount : Nothing,
  rideDuration : Just 0,
  isScheduled : false,
  vehicleServiceTierAirConditioned : Nothing,
  vehicleServiceTierSeatingCapacity : Nothing,
  returnTime : Nothing,
  isAirConditioned : Nothing
  }

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails = RideBookingDetails {
  toLocation : Nothing,
  estimatedDistance : Nothing,
  otpCode : Nothing,
  stopLocation : Nothing,
  senderDetails : Nothing,
  receiverDetails : Nothing,
  requestorPartyRoles : Nothing
}

fareUpdatePill :: ST.FareUpdatePill
fareUpdatePill = {
    text : ""
  , imageVis : VISIBLE
  , visible : GONE
}

riderRatingCard :: ST.RiderRatingCard
riderRatingCard = {
    rating : 0 
  , rideId : ""
  , isRecording : false
  , favDriver : false
  , distanceDifference : 0
  , feedbackList : []
  , feedbackPillData : []
  , recordAudioState : {
      isRecording : false 
    , timer : "00:00"
    , recordingDone : false
    , recordedFile: Nothing
    , openAddAudioModel: false
    , isUploading: false
    , uploadedAudioId : Nothing
    , recordedAudioUrl : Nothing
    , isListening : false
    , pauseLootie : false
  }
  , feedbackText : ""
}