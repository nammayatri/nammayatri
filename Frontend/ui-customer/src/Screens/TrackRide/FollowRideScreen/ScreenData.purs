module Screens.FollowRideScreen.ScreenData where

import ConfigProvider
import Data.Maybe
import Prelude
import Screens.HomeScreen.ScreenData
import Screens.Types
import PrestoDOM (BottomSheetState(..))
import Foreign.Object (empty)
import Services.API (Route(..), Snapped(..), LatLong(..), GetRouteResp(..), GetDriverLocationResp(..))
import Common.Types.App as CT
import Screens.Types (FareProductType(..)) as FPT
import Screens.Types as ST
import Components.MessagingView.Controller (dummyChatRecipient)

initData :: FollowRideScreenState
initData =
  { data:
      { driverInfoCardState: Nothing
      , currentStage: PersonList
      , currentFollower: Nothing
      , logField: empty
      , followers: []
      , zoneType: dummyZoneType
      , route: Nothing
      , speed: 1
      , config: getAppConfig appConfig
      , messages: []
      , messagesSize: "-1"
      , chatSuggestionsList: []
      , lastMessage: { message: "", messageTitle: Nothing, messageAction: Nothing, messageLabel : Nothing, sentBy: "", timeStamp: "", type: "", delay: 0 }
      , lastSentMessage: { message: "", sentBy: "", timeStamp: "", type: "", delay: 0 }
      , lastReceivedMessage: { message: "", sentBy: "", timeStamp: "", type: "", delay: 0 }
      , messageToBeSent: ""
      , sosStatus: Nothing
      , emergencyAudioStatus: STOPPED
      , counter: 0
      }
  , props:
      { city: CT.AnyCity
      , showChatNotification: false
      , canSendSuggestion: true
      , isChatNotificationDismissed: false
      , unReadMessages: false
      , removeNotification: true
      , enableChatWidget: false
      , chatCallbackInitiated: false
      , openChatScreen: false
      , sendMessageActive: false
      , sheetState: Nothing
      , currentSheetState: HALF_EXPANDED
      , isNotificationExpanded: false
      , startMapAnimation: true
      , isRideStarted: false
      , isMock : false
      , currentUserOnRide : false
      }
  }

dummyFollower :: Followers
dummyFollower =
  { name: Nothing
  , bookingId: ""
  , mobileNumber: ""
  , priority: 0
  , isManualFollower : false
  , personId : Nothing
  }

mockFollower :: Followers
mockFollower =
  { name: Nothing
  , bookingId: "mock_drill"
  , mobileNumber: ""
  , priority: 0
  , isManualFollower : false
  , personId : Nothing
  }

mockRoute :: Route
mockRoute =
  Route
    { boundingBox: Nothing
    , distance: 1671
    , duration: 150
    , pointsForRentals : Nothing
    , points:
        Snapped
          [ LatLong
              { lat: 12.942134
              , lon: 77.622155
              }
          , LatLong
              { lat: 12.942521
              , lon: 77.622629
              }
          , LatLong
              { lat: 12.942471
              , lon: 77.622682
              }
          , LatLong
              { lat: 12.940902
              , lon: 77.620634
              }
          , LatLong
              { lat: 12.939092
              , lon: 77.619282
              }
          , LatLong
              { lat: 12.93778
              , lon: 77.61863
              }
          , LatLong
              { lat: 12.937096
              , lon: 77.618389
              }
          , LatLong
              { lat: 12.936524
              , lon: 77.617828
              }
          , LatLong
              { lat: 12.936317
              , lon: 77.617436
              }
          , LatLong
              { lat: 12.936274
              , lon: 77.616317
              }
          , LatLong
              { lat: 12.936036
              , lon: 77.615656
              }
          , LatLong
              { lat: 12.934327
              , lon: 77.612295
              }
          , LatLong
              { lat: 12.934941
              , lon: 77.611986
              }
          ]
    , snappedWaypoints:
        Snapped
          [ LatLong
              { lat: 12.942134
              , lon: 77.622155
              }
          , LatLong
              { lat: 12.942521
              , lon: 77.622629
              }
          , LatLong
              { lat: 12.942446
              , lon: 77.622646
              }
          , LatLong
              { lat: 12.934369
              , lon: 77.612397
              }
          , LatLong
              { lat: 12.934941
              , lon: 77.611986
              }
          ]
    }

mockDriverLocation :: GetDriverLocationResp
mockDriverLocation =
  GetDriverLocationResp
    $ LatLong
        { lat: 12.942134
        , lon: 77.622155
        }

mockDriverInfo :: DriverInfoCard
mockDriverInfo =
  { otp: ""
  , driverName: "Test Driver"
  , eta: Nothing
  , vehicleDetails: ""
  , registrationNumber: "XXXXXXXXXX"
  , rating: 5.0
  , startedAt: ""
  , endedAt: ""
  , source: "NA"
  , destination: "NA"
  , rideId: ""
  , price: 0
  , sourceLat: 12.942134
  , sourceLng: 77.622155
  , destinationLat: 12.934941
  , destinationLng: 77.611986
  , driverLat: 0.0
  , driverLng: 0.0
  , initialPickupLat : 0.0
  , initialPickupLon : 0.0
  , distance: 0
  , waitingTime: "--"
  , driverArrived: false
  , estimatedDistance: ""
  , driverArrivalTime: 0
  , destinationReachedAt : 0
  , destinationReached : false
  , bppRideId: ""
  , driverNumber: Nothing
  , merchantExoPhone: ""
  , createdAt: ""
  , initDistance: Nothing
  , config: getAppConfig appConfig
  , vehicleVariant: ""
  , sourceAddress: dummyAddress
  , destinationAddress: dummyAddress
  , editPickupAttemptsLeft : 0
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
  , currentChatRecipient : dummyChatRecipient
  , hasToll : false
  , isAlreadyFav : false
  , favCount : 0
  , rideDuration : Just 0
  , rideScheduledAtUTC : Nothing
  , senderDetails : Nothing
  , receiverDetails : Nothing
  , estimatedTimeToReachDestination : Nothing
  , isAirConditioned : Nothing
  }
