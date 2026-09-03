{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.ScreenData where

import Screens.Types as ST
import ConfigProvider (getAppConfig, appConfig)
import Data.Array as Array
import Data.Function.Uncurried (runFn3)
import Data.Maybe as Mb
import DecodeUtil (getAnyFromWindow)
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Prelude ((==), ($))
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption, alwaysShareRideOption, shareWithTimeContraintsRideOption)
import Services.API as API
import Screens.HomeScreen.ScreenData
import Common.Types.App as CT
import Screens.Types (FareProductType(..)) as FPT
import Components.MessagingView.Controller (dummyChatRecipient)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Prelude (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Screens(ScreenName(..), getScreen)
import Foreign.Object (empty)

initData :: ST.BusTrackingScreenState
initData =
  let
    appConfig' = getAppConfig appConfig
  in
    { data:
        { appConfig: appConfig'
        , busRouteCode: ""
        , stopsList: []
        , sourceStation: Mb.Nothing
        , destinationStation: Mb.Nothing
        , bookingId: ""
        , vehicleTrackingData: DM.empty
        , previousStopsMap: DM.empty
        , rideType : Nothing
        , vehicleData : []
        , stationResponse : Mb.Nothing
        , routeShortName : ""
        , routePts : {points : []}
        , previousLatLonsOfVehicle: DM.empty
        , nearestStopFromCurrentLoc : Mb.Nothing
        , logField : empty
        , isNoBusAvailable : false
        }
    , props:
        { showRouteDetailsTab: true
        , expandStopsView: true
        , verticalLineHeight: 0
        , srcLat: 0.0
        , srcLon: 0.0
        , busNearSourceData: Mb.Nothing
        , gotMapReady : false
        , showShimmer : true
        , individualBusTracking : false
        , vehicleTrackingId : Mb.Nothing
        , userAndBuslocationMatchCount : 0
        , previousScreen : ST.BusHomeScreen
        , destinationSequenceNumber : Mb.Nothing
        , currentLat : 0.0
        , currentLon : 0.0
        , minimumEtaDistance : Nothing
        , isMinimumEtaDistanceAvailable : Nothing
        , fromScreen : getScreen BUS_ROUTE_STOPS_SEARCH_SCREEN
        , showBikeTaxiPopUp : false
        , isBikeTaxiCrossClicked : false
        }
    }

mockDriverLocation :: API.GetDriverLocationResp
mockDriverLocation =
  API.GetDriverLocationResp
    $ API.LatLong
        { lat: 12.942134
        , lon: 77.622155
        }

mockDriverInfo :: ST.DriverInfoCard
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

mockRoute :: API.Route
mockRoute =
  API.Route
    { boundingBox: Mb.Nothing
    , distance: 1671
    , duration: 150
    , pointsForRentals: Mb.Nothing
    , points:
        API.Snapped
          [ API.LatLong
              { lat: 12.942134
              , lon: 77.622155
              }
          , API.LatLong
              { lat: 12.942521
              , lon: 77.622629
              }
          , API.LatLong
              { lat: 12.942471
              , lon: 77.622682
              }
          , API.LatLong
              { lat: 12.940902
              , lon: 77.620634
              }
          , API.LatLong
              { lat: 12.939092
              , lon: 77.619282
              }
          , API.LatLong
              { lat: 12.93778
              , lon: 77.61863
              }
          , API.LatLong
              { lat: 12.937096
              , lon: 77.618389
              }
          , API.LatLong
              { lat: 12.936524
              , lon: 77.617828
              }
          , API.LatLong
              { lat: 12.936317
              , lon: 77.617436
              }
          , API.LatLong
              { lat: 12.936274
              , lon: 77.616317
              }
          , API.LatLong
              { lat: 12.936036
              , lon: 77.615656
              }
          , API.LatLong
              { lat: 12.934327
              , lon: 77.612295
              }
          , API.LatLong
              { lat: 12.934941
              , lon: 77.611986
              }
          ]
    , snappedWaypoints:
        API.Snapped
          [ API.LatLong
              { lat: 12.942134
              , lon: 77.622155
              }
          , API.LatLong
              { lat: 12.942521
              , lon: 77.622629
              }
          , API.LatLong
              { lat: 12.942446
              , lon: 77.622646
              }
          , API.LatLong
              { lat: 12.934369
              , lon: 77.612397
              }
          , API.LatLong
              { lat: 12.934941
              , lon: 77.611986
              }
          ]
    }

data StopType = SOURCE_STOP | DESTINATION_STOP | NORMAL_STOP | ROUTE_SOURCE | ROUTE_END

derive instance genericStopType :: Generic StopType _
instance eqStopType :: Eq StopType where eq = genericEq