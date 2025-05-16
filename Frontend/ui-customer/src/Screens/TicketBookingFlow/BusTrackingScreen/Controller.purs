{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.Controller where

import PrestoDOM.Core (getPushFn)
import Engineering.Helpers.Commons as EHC
import Data.Traversable (traverse, for_, for)
import JBridge as JB
import JBridge (firebaseLogEvent)
import Data.Time.Duration (Milliseconds(..))
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import Debug
import Prelude
import PrestoDOM
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData
import Helpers.Utils as HU
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM.Core (processEvent)
import Screens.Types as ST
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.BannerCarousel as BannerCarousel
import RemoteConfig as RC
import Data.String as DS
import Effect.Unsafe (unsafePerformEffect)
import Locale.Utils (getLanguageLocale)
import Constants (languageKey)
import SessionCache (getValueFromWindow)
import Data.Array as DA
import Data.Int as DI
import Data.Number as DN
import Data.Maybe as Mb
import Data.Ord (comparing)
import PrestoDOM.List (ListItem)
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader.Controller as DropDownWithHeader
import Components.PrimaryButton as PrimaryButton
import Services.API as API
import Styles.Colors as Color
import Helpers.API as HelpersAPI
-- import Screens.MultiModalFlow.Components.MetroCard as MetroCard
-- import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
import Effect.Aff (launchAff)
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (GlobalState(..), defaultGlobalState)
import Common.Types.App as CTA
import Data.Lens ((^.))
import Accessor (_lat, _lon, _stopPoint, _sequenceNum, _stopCode)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Constants.Configs (getPolylineAnimationConfig)
import Screens.TicketBookingFlow.BusTrackingScreen.Transformer
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Either
import Data.Map as DM
import Data.Newtype (unwrap)
import Data.Tuple as DT
import Data.Foldable (foldM, minimum, minimumBy)
import Engineering.Helpers.RippleCircles as EHR
import Effect.Uncurried (runEffectFn1, runEffectFn3)
import Foreign.Class (encode)
import Storage (setValueToLocalStore)
import Data.Argonaut.Core as AC
import DecodeUtil as DU
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import Control.Alt ((<|>))
import Common.RemoteConfig.Utils as RU
import Common.Types.App as CT
import MapUtils (calculateNearestWaypoint, rotationBetweenLatLons, haversineDistance, calculateVehicleBearingViaRoute)
import Data.Tuple (Tuple(..))
import Common.Resources.Constants (zoomLevel, chatService)
import Data.Set as DSet
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Screens (getScreen, ScreenName(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.LogEvent (logEventWithMultipleParams)
import Presto.Core.Types.Language.Flow (getLogFields)
import Control.Monad.Except.Trans (lift)
import Foreign (MultipleErrors, unsafeToForeign)
import Engineering.Helpers.LogEvent (firebaseLogEventWithArrayOfKeyValue)
import Engineering.Helpers.Events as Events


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data ScreenOutput
  = Exit ST.BusTrackingScreenState
  | GoToBusTicketBooking ST.BusTrackingScreenState
  | GoToSearchLocation ST.BusTrackingScreenState
  | GoToViewTicket ST.BusTrackingScreenState
  | GoBackToSearchLocationScreen ST.BusTrackingScreenState
  | GoToBusTicketBookingScreen ST.BusTrackingScreenState
  | GoBackToMetroMyTicketsScreen ST.BusTrackingScreenState

data Action
  = AfterRender
  | BackPressed
  | CurrentLocationCallBack String String String
  | UpdateTracking (API.BusTrackingRouteResp) Int String
  | MapReady String String String
  | NoAction
  | BookTicketButtonAction PrimaryButton.Action
  | ToggleStops
  | UpdateStops API.GetMetroStationResponse
  | ViewTicket
  | UserBoarded (Mb.Maybe String)
  | SaveRoute JB.Locations
  | UpdateToExpandView 

-- | API.BusTrackingRouteResp
eval :: Action -> ST.BusTrackingScreenState -> Eval Action ScreenOutput ST.BusTrackingScreenState
eval (MapReady _ _ _) state =
  continueWithCmd state { props { gotMapReady = true } }
    [ do
        void $ launchAff $ EHC.flowRunner defaultGlobalState
          $ do
              case state.data.stationResponse of
                (Mb.Just stations) -> drawDriverRoute stations state
                _ -> pure unit
        pure NoAction
    ]

eval ToggleStops state = do
  continue state { props { expandStopsView = not state.props.expandStopsView } }

eval (UpdateStops (API.GetMetroStationResponse metroResponse)) state =
  let
    filteredResp = -- metroResponse
    -- Right now showing all stops event though user has boooked a ticket from pickup and destination stop
      case state.data.destinationStation, DS.null state.data.bookingId, state.props.vehicleTrackingId of
        Mb.Just dest, false, _ -> do
          case DA.findIndex (\(API.FRFSStationAPI x) -> x.code == dest.stationCode) metroResponse of
            Mb.Nothing -> metroResponse -- If the item isn't found, return the original array
            Mb.Just index -> DA.take (index + 1) metroResponse
        _, _, _-> metroResponse
    
    destinationStationSeq = Mb.maybe Mb.Nothing (\(API.FRFSStationAPI dest) -> dest.sequenceNum)(DA.last filteredResp)

    finalMap =
      DA.foldl
        ( \acc item@(API.FRFSStationAPI station) -> case acc.previousStop of
            Mb.Nothing -> acc { previousStop = Mb.Just item }
            Mb.Just stop -> acc { outputMap = DM.insert station.code stop acc.outputMap, previousStop = Mb.Just item }
        )
        { outputMap: DM.empty, previousStop: Mb.Nothing }
        filteredResp
    
    -- Nearest stop calculation from Current Location
    nearestStopFromCurrentLoc = 
      getNearestStopFromLatLong (API.LatLong {lat : state.props.srcLat, lon: state.props.srcLon}) filteredResp
  
  in continueWithCmd state { data { stopsList = filteredResp, previousStopsMap = finalMap.outputMap, stationResponse = Mb.Just metroResponse, nearestStopFromCurrentLoc = nearestStopFromCurrentLoc}, props { showShimmer = false, destinationSequenceNumber = destinationStationSeq } }
      [ do
          void $ launchAff $ EHC.flowRunner defaultGlobalState
            $ do
                when state.props.gotMapReady $ drawDriverRoute metroResponse state
                pure unit
          pure NoAction
      ]

eval (BookTicketButtonAction PrimaryButton.OnClick) state = do
  let _ = unsafePerformEffect $ Events.addEventAggregate "ny_bus_user_book_ticket_initiated"
  exit $ GoToSearchLocation state

eval BackPressed state =
    case state.props.fromScreen of
      "bus_ticket_booking_screen" -> exit $ GoToBusTicketBookingScreen state
      "metro_my_tickets_screen" -> exit $ GoBackToMetroMyTicketsScreen state
      _ -> exit $ GoBackToSearchLocationScreen state

eval ViewTicket state = exit $ GoToViewTicket state

eval (UpdateTracking (API.BusTrackingRouteResp resp) count cachedBusOnboardingInfoString) state =
  let trackingData = DA.concatMap extractTrackingInfo resp.vehicleTrackingInfo
  in 
    case state.props.vehicleTrackingId of
      Mb.Just id -> do
        let vehicleDetails = DA.find (\item -> item.vehicleId == id) trackingData
        case vehicleDetails of 
          Mb.Just details -> do
            continueWithCmd state [do
              void $ launchAff $ EHC.flowRunner defaultGlobalState $ userBoardedActions state trackingData details
              pure NoAction
            ]
          Mb.Nothing -> processTrackingData trackingData
      Mb.Nothing -> processTrackingData trackingData
  where
    processTrackingData trackingData = do
      let
        cachedBusOnboardingInfo = extractBusOnboardingInfo cachedBusOnboardingInfoString
        alreadyOnboardedThisBus = DA.find (\busInfo -> busInfo.bookingId == state.data.bookingId) $ cachedBusOnboardingInfo
        _ = spy "Bus Onboarding Cached data" $ cachedBusOnboardingInfo
        _ = spy "Bus Onboarding data " $ Tuple state.data.bookingId $ cachedBusOnboardingInfo
        _ = spy "Already onboarded this bus" $ Tuple state.data.bookingId alreadyOnboardedThisBus
        finalMap =
          DA.foldl
            ( \acc item -> do
                let alreadyOnboardedThisVehicleInRouteOrPreTracking = Mb.isNothing alreadyOnboardedThisBus || (alreadyOnboardedThisBus <#> _.vehicleId) == Mb.Just item.vehicleId
                if (((Mb.maybe false (_ < item.nextStopSequence) state.props.destinationSequenceNumber && DS.null state.data.bookingId) || item.vehicleId == "")) -- && alreadyOnboardedThisVehicleInRouteOrPreTracking
                  then acc
                else do
                  let
                    mbPreviousStop = DM.lookup item.nextStop state.data.previousStopsMap
                    zoomIn = (item.nextStop == (Mb.maybe "" (_.stationCode) state.data.sourceStation))
                  case mbPreviousStop of 
                    Mb.Nothing -> acc
                    Mb.Just (API.FRFSStationAPI previousStop) -> do
                      let
                        distanceFromPreviousStop = HU.getDistanceBwCordinates (Mb.fromMaybe 0.0 previousStop.lat) (Mb.fromMaybe 0.0 previousStop.lon) item.vehicleLat item.vehicleLon

                        travelDistancePercent = item.nextStopDistance / (distanceFromPreviousStop + item.nextStopDistance)

                        currentValues = Mb.fromMaybe [] $ DM.lookup item.nextStop (DT.fst acc)
                        isBusNearer = item.nextStopDistance < (Mb.fromMaybe 1000000.0 $ (DT.snd acc) <#> _.nextStopDistance)
                      DT.Tuple (DM.insert item.nextStop (currentValues <> [ travelDistancePercent ]) (DT.fst acc))
                        ( if (zoomIn && ((Mb.isNothing $ DT.snd acc) || isBusNearer)) then
                            Mb.Just item
                          else
                            DT.snd acc
                        )
            )
            (DT.Tuple DM.empty Mb.Nothing)
            trackingData

      let nearByBusPosition =  Mb.maybe Mb.Nothing (\item -> Mb.Just {lat : item.vehicleLat, lng : item.vehicleLon}) (DT.snd finalMap)
      case alreadyOnboardedThisBus of
        Mb.Just busInfo ->
          continueWithCmd state { data { vehicleTrackingData = DT.fst finalMap, vehicleData = trackingData }, props { individualBusTracking = true, vehicleTrackingId = Mb.Just busInfo.vehicleId } }
            [ pure $ UserBoarded (Mb.Just busInfo.vehicleId) ]
        Mb.Nothing -> 
          continueWithCmd state
            { data { vehicleTrackingData = DT.fst finalMap, vehicleData = trackingData, previousLatLonsOfVehicle = storePrevLatLonsOfVehicle trackingData}
            , props
              { busNearSourceData = DT.snd finalMap
              , minimumEtaDistance = calculateMinETADistance trackingData
              , isMinimumEtaDistanceAvailable = 
                  if (count == 0) then Mb.Nothing else Mb.Just $ Mb.isJust $ calculateMinETADistance trackingData
              }
            }
          [ do
              void $ launchAff $ EHC.flowRunner defaultGlobalState
                $ do
                    when state.props.gotMapReady $ updateBusLocationOnRoute state trackingData $ API.BusTrackingRouteResp resp
              if (count == 1) then do
                let etaToStore =show $ Mb.fromMaybe 0 $ calculateMinETADistance trackingData
                    params = [Tuple "Eta" etaToStore]
                let _ = unsafePerformEffect $ Events.addEventData "External.WMB.ny_bus_user_Eta_seen" etaToStore
                let vehicleTrackingDataSize = DA.length trackingData
                let _ = unsafePerformEffect $ Events.addEventData "External.WMB.ny_bus_tracking_count" (show $ vehicleTrackingDataSize)
                let _ = unsafePerformEffect $ Events.addEventData "External.WMB.ny_bus_minimum_eta_distance" (show $ state.props.minimumEtaDistance)
                pure unit
              else
                pure unit
              
              case DT.snd finalMap of
                Mb.Just pt -> do
                  void $ JB.animateCamera pt.vehicleLat pt.vehicleLon 15.0 "ZOOM"
                Mb.Nothing -> pure unit
              pure NoAction
          ]
    storePrevLatLonsOfVehicle trackingData =
      DA.foldl
        (\acc item ->
          DM.insert item.vehicleId { position : API.LatLong {lat : item.vehicleLat, lon: item.vehicleLon}, index : item.nearestWaypointConfig.index } acc
        )
        DM.empty
        trackingData
    
    extractTrackingInfo :: API.VehicleInfo -> Array ST.VehicleData
    extractTrackingInfo (API.VehicleInfo item) = do
      let 
        (API.VehicleInfoForRoute vehicleInfoForRoute) = item.vehicleInfo
        -- (API.RouteStopMapping nextStop) = item.nextStop
        lat = Mb.fromMaybe 0.0 vehicleInfoForRoute.latitude
        lon = Mb.fromMaybe 0.0 vehicleInfoForRoute.longitude
        -- (API.LatLong nextStopPosition) = nextStop.stopPoint
        currentWaypoint = API.LatLong { lat: lat, lon: lon }
        wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
        nearestWaypointConfig = calculateNearestWaypoint currentWaypoint state.data.routePts.points wmbFlowConfig.maxSnappingOnRouteDistance
        mbPickupStop = 
          case state.data.sourceStation of
            Mb.Just sourceStation -> DA.find (\(API.FRFSStationAPI item) -> item.code == sourceStation.stationCode) state.data.stopsList
            Mb.Nothing -> state.data.nearestStopFromCurrentLoc
        (Tuple pickupPoint mbSequenceNum) = fetchPickupPoint (API.VehicleInfo item) mbPickupStop

      case filterVehicleInfoLogic (API.VehicleInfo item) wmbFlowConfig, (filterSnappedWaypoint nearestWaypointConfig wmbFlowConfig (API.VehicleInfo item)) of
        false, _ -> []
        _, false -> []
        true, true ->
          let 
            upcomgingStops = Mb.fromMaybe [] vehicleInfoForRoute.upcomingStops
            -- Proper Checking to see if the bus has moved backward in route then not counting that as a valid location
            vehicleLatLong =
              ( case (DM.lookup item.vehicleId state.data.previousLatLonsOfVehicle) of 
                  Mb.Just previousVehicleData ->
                    let distanceBtwnCurrentAndPrevLocations = haversineDistance previousVehicleData.position nearestWaypointConfig.vehicleLocationOnRoute
                        currentLatLonBeforePrevIndex = nearestWaypointConfig.index >= previousVehicleData.index
                    in 
                      if ((not currentLatLonBeforePrevIndex) || distanceBtwnCurrentAndPrevLocations >= wmbFlowConfig.maxDeviatedDistanceInMeters)
                        then previousVehicleData.position
                        else nearestWaypointConfig.vehicleLocationOnRoute
                  Mb.Nothing -> nearestWaypointConfig.vehicleLocationOnRoute
              )
            -- Proper calculation of eta from Upcoming Stops
            etaTillPickupStop = DA.find (\(API.UpcomingStop upcomingStop) ->
                let (API.Stop stop) = upcomingStop.stop
                in (mbPickupStop <#> (\(API.FRFSStationAPI frfsStop) -> frfsStop.code)) == (Mb.Just $ stop.stopCode)
              ) upcomgingStops

            -- Calculate last reached Stop
            lastReachedStop = DA.find (\(API.UpcomingStop upcomingStop) -> upcomingStop.status == "Reached") $ DA.reverse upcomgingStops

            -- Extract first upcoming stop and next stop details
            firstUpcomingStop = DA.find (\(API.UpcomingStop upcomingStop) -> upcomingStop.status == "Upcoming") upcomgingStops
            upcomingNextStop = firstUpcomingStop <#> (\(API.UpcomingStop upcomingStop) -> upcomingStop.stop)
            nextStopLatLon = Mb.fromMaybe (API.LatLong {lat: 0.0, lon: 0.0}) $ upcomingNextStop <#> (\(API.Stop stop) -> stop.coordinate) -- (API.LatLong nextStopPosition)
            nextStopIdx = Mb.fromMaybe (0) $ upcomingNextStop <#> (\(API.Stop stop) -> stop.stopIdx) -- nextStop.sequenceNum

            -- Proper ETA Distance Calculation from upComingNextStop
            calculatedETADistance = DA.foldl
              (\acc (API.UpcomingStop upcomingStop) ->
                let (API.Stop stop) = upcomingStop.stop
                in
                  if (stop.stopIdx < (Mb.fromMaybe 0 mbSequenceNum) && upcomingStop.status == "Upcoming") then
                    acc + stop.distanceToUpcomingIntermediateStop
                  else
                    acc
              )
              0
              upcomgingStops

            -- deltaFinal =  firstUpcomingStop !! lastReachedDelta
          in 
            [ { vehicleId: item.vehicleId
            , timestamp: vehicleInfoForRoute.timestamp
            , nextStop: Mb.fromMaybe "" $ upcomingNextStop <#> (\(API.Stop stop) -> stop.stopCode)
            , nextStopDistance: Mb.fromMaybe (haversineDistance vehicleLatLong $ nextStopLatLon) $ upcomingNextStop <#> (\(API.Stop stop) -> DI.toNumber stop.distanceToUpcomingIntermediateStop)
            , vehicleLat: vehicleLatLong ^._lat
            , vehicleLon: vehicleLatLong ^._lon
            , nextStopLat: nextStopLatLon ^._lat
            , nextStopLon: nextStopLatLon ^._lon
            , nextStopSequence: nextStopIdx
            , nearestWaypointConfig: nearestWaypointConfig
            , etaDistance: if (calculatedETADistance /= 0 && nextStopIdx < (Mb.fromMaybe 0 mbSequenceNum)) then Mb.Just $ DI.toNumber calculatedETADistance else Mb.Nothing -- if nextStopIdx < (Mb.fromMaybe 0 mbSequenceNum) then (Mb.Just $ haversineDistance vehicleLatLong pickupPoint) else Mb.Nothing
            , eta: filterETAForOlderLatLong vehicleInfoForRoute.timestamp $ Mb.maybe Mb.Nothing (\stop -> calculateETAFromUpcomingStop lastReachedStop stop item.vehicleId) etaTillPickupStop -- EHC.compareUTCDate  (EHC.getCurrentUTC "")
            , delta : firstUpcomingStop <#> (\(API.UpcomingStop stop) -> Mb.fromMaybe 0.0 stop.delta)
            } ]
     
    -- calLastReachedDelta (API.UpcomingStop upcomingStop) =
    --   let (API.Stop stop) = upcomingStop.stop
    --   in stop.stopIdx
    filterETAForOlderLatLong timeStamp eta' = 
      let timeDiff = EHC.compareUTCDate (EHC.getCurrentUTC "") $ timeStamp
      in if (timeDiff < 120) then eta' else Mb.Nothing

    calculateETAFromUpcomingStop lastReachedStop (API.UpcomingStop stop) vehicleId = 
      let etaTimeStamp = stop.eta
          delayInSeconds = DI.round $ Mb.fromMaybe 0.0 stop.delta
            --DI.floor $ Mb.fromMaybe 0.0 $ lastReachedStop <#> (\(API.UpcomingStop stop) -> Mb.fromMaybe 0.0 stop.delta)
          etaInSeconds = EHC.compareUTCDate etaTimeStamp (EHC.getCurrentUTC "")
          -- _ = spy "UTC TimeStamp of ETA and currentTime " $ Tuple etaTimeStamp (EHC.getCurrentUTC "")
          -- _ = spy "ETA in seconds and the delat" $ Tuple etaInSeconds delayInSeconds
          -- _ = spy "Proper ETA with Delay" $ Tuple vehicleId (etaInSeconds + delayInSeconds)
      in if etaInSeconds + delayInSeconds > 0
        then Mb.Just $ etaInSeconds + delayInSeconds
        else Mb.Nothing 

    filterVehicleInfoLogic (API.VehicleInfo item) wmbFlowConfig =
      let (API.VehicleInfoForRoute m) = item.vehicleInfo
          -- Show Bus numbers whose rides haven't been ended even though last LTS update is > 30 mins ago
          -- _ = spy "Vehicle Time Diff" $ Tuple item.vehicleId timeDiff 
          timeDiff = EHC.compareUTCDate (EHC.getCurrentUTC "") m.timestamp
      in (timeDiff < wmbFlowConfig.maxAllowedTimeDiffInLTSinSec) && checkCurrentBusIsOnboarded state item.vehicleId

    checkCurrentBusIsOnboarded state vehicleId = 
      let cachedBusOnboardingInfo = extractBusOnboardingInfo cachedBusOnboardingInfoString
      in (DA.null cachedBusOnboardingInfo || (not state.props.individualBusTracking) || (Mb.isJust $ DA.find (\busInfo -> busInfo.vehicleId == vehicleId) cachedBusOnboardingInfo))

    filterSnappedWaypoint nearestWaypointConfig wmbFlowConfig (API.VehicleInfo item) =
      -- Deviation Filter Logic for Bus Vehicles
      ((nearestWaypointConfig.deviationDistance < wmbFlowConfig.maxDeviatedDistanceInMeters) || wmbFlowConfig.showAllDeviatedBus)
    
    fetchPickupPoint :: API.VehicleInfo -> Mb.Maybe API.FRFSStationAPI -> Tuple API.LatLong (Mb.Maybe Int)
    fetchPickupPoint (API.VehicleInfo item) pickupStop =
      let 
        pickupLat = Mb.maybe state.props.srcLat (\(API.FRFSStationAPI item) -> Mb.fromMaybe state.props.srcLat item.lat) pickupStop
        pickupLon = Mb.maybe state.props.srcLon (\(API.FRFSStationAPI item) -> Mb.fromMaybe state.props.srcLat item.lon) pickupStop
      in Tuple (API.LatLong { lat: pickupLat, lon: pickupLon }) (Mb.maybe Mb.Nothing (\(API.FRFSStationAPI item) -> item.sequenceNum) pickupStop)
        -- Mb.Nothing -> 
        --   let nearestStopFromCurrentLoc = state.data.nearestStopFromCurrentLoc
        --       nearestStopLat = Mb.maybe state.props.srcLat (\(API.FRFSStationAPI item) -> Mb.fromMaybe state.props.srcLat item.lat) nearestStopFromCurrentLoc
        --       nearestStopLon = Mb.maybe state.props.srcLon (\(API.FRFSStationAPI item) -> Mb.fromMaybe state.props.srcLat item.lon) nearestStopFromCurrentLoc
        --   in Tuple $ API.LatLong { lat: nearestStopLat, lon: nearestStopLon }

eval (CurrentLocationCallBack lat lon _) state = case state.props.busNearSourceData of
    Mb.Just location -> do
      let distance = HU.getDistanceBwCordinates (parseCoordinate lat) (parseCoordinate lon) location.vehicleLat location.vehicleLon
      if distance < 0.050 && state.props.userAndBuslocationMatchCount + 1 >= 5
        then continue state{props{individualBusTracking = true, userAndBuslocationMatchCount = state.props.userAndBuslocationMatchCount + 1, vehicleTrackingId = state.props.busNearSourceData <#> _.vehicleId}}
      else if distance < 0.050 then continue state{props{userAndBuslocationMatchCount = state.props.userAndBuslocationMatchCount + 1}}
      else continue state {props{userAndBuslocationMatchCount = 0}}
    Mb.Nothing -> continue state
  where
    parseCoordinate pt = Mb.fromMaybe 0.0 $ DN.fromString pt

eval (UserBoarded mbVehicleId) state = do
  let filterStopsIndex = DA.findIndex (\(API.FRFSStationAPI item) -> item.code == sourceCode) state.data.stopsList
      sourceCode = Mb.fromMaybe "" $ state.data.sourceStation <#> _.stationCode
      filteredStops = case filterStopsIndex of 
        Mb.Just index -> DA.slice index (DA.length state.data.stopsList) state.data.stopsList
        Mb.Nothing -> state.data.stopsList
      -- findBusInfo = DA.find (\busInfo -> busInfo.bookingId == state.data.bookingId) extractBusOnboardingInfo
      nearbyBusVehicleId = state.props.busNearSourceData <#> _.vehicleId
      vId = nearbyBusVehicleId <|> state.props.vehicleTrackingId
  continueWithCmd state {props{individualBusTracking = true, vehicleTrackingId = vId, expandStopsView = if Mb.isNothing mbVehicleId then true else state.props.expandStopsView}, data{stopsList = filteredStops}} [ do
    -- void $ launchAff $ EHC.flowRunner defaultGlobalState $ userBoardedActions state
    if (Mb.isNothing mbVehicleId && Mb.isJust vId && state.data.bookingId /= "" && Mb.isNothing state.props.vehicleTrackingId && Mb.isJust nearbyBusVehicleId) 
      -- then void $ pure $ setValueToLocalStore ONBOARDED_VEHICLE_INFO $ DU.stringifyJSON updatedOnboardingVehicleInfo
      then void $ pure $ setValueToCache (show ONBOARDED_VEHICLE_INFO) updatedOnboardingVehicleInfo (DU.stringifyJSON <<< encode)
      else pure unit
    pure NoAction
  ]
  where
    updatedOnboardingVehicleInfo :: ST.OnboardedBusInfo
    updatedOnboardingVehicleInfo = 
      case decodedOnboardedBusInfo of
        Mb.Nothing -> vehicleInfoPerBookingId
        Mb.Just info -> info <> vehicleInfoPerBookingId

    decodedOnboardedBusInfo :: Mb.Maybe ST.OnboardedBusInfo
    decodedOnboardedBusInfo = 
      let cachedOnboardedBusInfo = JB.getKeyInSharedPrefKeys $ show ONBOARDED_VEHICLE_INFO
      in DU.decodeForeignAny (DU.parseJSON cachedOnboardedBusInfo) Mb.Nothing

    vehicleInfoPerBookingId :: ST.OnboardedBusInfo 
    vehicleInfoPerBookingId = [{ bookingId: state.data.bookingId, vehicleId: Mb.fromMaybe "" $ state.props.busNearSourceData <#> _.vehicleId }]

eval (SaveRoute route) state = continue state {data{routePts = route}}

eval UpdateToExpandView state = continue state {props{expandStopsView = true}}

eval _ state = update state

drawDriverRoute :: Array API.FRFSStationAPI -> ST.BusTrackingScreenState -> Flow GlobalState Unit
drawDriverRoute resp state = do
  let
    srcCode = Mb.maybe "" (_.stationCode) state.data.sourceStation

    destinationCode = Mb.maybe "" (_.stationCode) state.data.destinationStation
  response <- HelpersAPI.callApi $ API.FrfsGetRouteReq state.data.busRouteCode (getValueToLocalStore CUSTOMER_LOCATION) "BUS"
  let
    route =
      Remote.walkCoordinates $ API.Snapped
        $ case response of
            Right (API.FRFSRouteAPI routeResp) -> Mb.fromMaybe [] routeResp.waypoints
            Left err -> []
    destinationStation = DA.find (\(API.FRFSStationAPI item) -> item.code == destinationCode) resp
  filteredRoute <- case destinationStation, DS.null state.data.bookingId of
    Mb.Just (API.FRFSStationAPI dest), false -> do
      locationResp <- EHC.liftFlow $ JB.isCoordOnPath ({points: DA.reverse route.points}) (Mb.fromMaybe 0.0 dest.lat) (Mb.fromMaybe 0.0 dest.lon) 1
      pure $ if locationResp.isInPath then { points: DA.reverse locationResp.points } else route
    _, _ -> pure route
  push <- EHC.liftFlow $ getPushFn Mb.Nothing "BusTrackingScreen"
  EHC.liftFlow $ push $ SaveRoute filteredRoute
  let
    routeConfig = transformStationsForMap resp filteredRoute srcCode destinationCode
  void $ pure $ JB.removeAllPolylines ""
  void $ pure $ JB.removeAllMarkers ""
  EHC.liftFlow $ JB.drawRoute [ routeConfig {routeWidth = 10} ] (EHC.getNewIDWithTag "BusTrackingScreenMap")

  -- case destinationStation, sourceStation, DS.null state.data.bookingId of
  --   Mb.Just (API.FRFSStationAPI dest), Mb.Just(API.FRFSStationAPI src), false -> do
  --     -- Not Showing route till just the stop location
  --     -- locationResp <- EHC.liftFlow $ JB.isCoordOnPath ({points: DA.reverse route.points}) (Mb.fromMaybe 0.0 dest.lat) (Mb.fromMaybe 0.0 dest.lon) 1
  --     -- pure $ if locationResp.isInPath then { points: locationResp.points } else route
      
  --     -- Breaking route drawing into 2 parts on top of each other
  --     EHC.liftFlow $ JB.drawRoute [ routeConfig {routeWidth = 10, routeColor = "#919191"} ] (EHC.getNewIDWithTag "BusTrackingScreenMap")

  --     sourceToDestinationRoute <- EHC.liftFlow $ JB.isCoordOnPath ({points: DA.reverse route.points}) (Mb.fromMaybe 0.0 dest.lat) (Mb.fromMaybe 0.0 dest.lon) 1
  --     pickupToDestinationRoute <- EHC.liftFlow $ JB.isCoordOnPath ({points: DA.reverse sourceToDestinationRoute.points}) (Mb.fromMaybe 0.0 src.lat) (Mb.fromMaybe 0.0 src.lon) 1 
  --     EHC.liftFlow $ JB.drawRoute [ routeConfig {routeWidth = 8, routeColor = Color.black900, locations = {points : pickupToDestinationRoute.points} }] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  --   _, _, _ ->
  --     EHC.liftFlow $ JB.drawRoute [ routeConfig {routeWidth = 10} ] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  
  EHC.liftFlow $ JB.setMapPadding 0 0 0 300
  void $ foldM processStop 0 state.data.stopsList
  where
  processStop index (API.FRFSStationAPI item) = do
    let
      lat = Mb.fromMaybe 0.0 item.lat
      lon = Mb.fromMaybe 0.0 item.lon
      markerId = item.code
      pointertype = getStopType item.code index state
      size = getStopMarkerSize pointertype state.data.rideType index
      markerZIndex = getZIndex pointertype
    void $ EHC.liftFlow $ JB.showMarker JB.defaultMarkerConfig { markerId = item.code, pointerIcon = getStopMarker pointertype state.data.rideType index, primaryText = item.name, markerSize = DI.toNumber size, zIndex = markerZIndex } lat lon size 0.5 (if pointertype == NORMAL_STOP then 0.5 else 0.9) (EHC.getNewIDWithTag "BusTrackingScreenMap")
    when (DA.elem pointertype [SOURCE_STOP, DESTINATION_STOP] && (state.data.rideType == Mb.Just ST.STOP || index /= 0)) $ EHC.liftFlow $ runEffectFn1 
      EHR.upsertMarkerLabel 
        { id: item.code <> "label"
        , title: item.name
        , actionImage: ""
        , actionCallBack: ""
        , position: {lat : lat , lng : lon}
        , markerImage : ""
        }
    pure (index + 1)

getPoint :: API.GetDriverLocationResp -> CTA.Paths
getPoint (API.GetDriverLocationResp resp) = { lat: resp ^. _lat, lng: resp ^. _lon }

extractBusOnboardingInfo :: String -> ST.OnboardedBusInfo
extractBusOnboardingInfo cachedBusOnboardingInfoString = 
  Mb.fromMaybe [] $ DU.decodeForeignAny (DU.parseJSON cachedBusOnboardingInfoString) Mb.Nothing

updateBusLocationOnRoute :: ST.BusTrackingScreenState -> Array ST.VehicleData -> API.BusTrackingRouteResp -> Flow GlobalState Unit
updateBusLocationOnRoute state vehicles (API.BusTrackingRouteResp resp)= do
  for_ vehicles
    $ \(item) -> do
        let pointerIcon = "ny_ic_bus_nav_on_map"
            markerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId, pointerIcon = pointerIcon , markerSize = 70.0, zIndex = 0.1}
            srcHeaderArrowMarkerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId <> "arrow_marker", pointerIcon = "ny_ic_nav_on_map_yellow_arrow" , markerSize = 105.0, zIndex = 0.0}
            vehicleRotationFromPrevLatLon = 
              case calculateVehicleBearingViaRoute (API.LatLong {lat: item.vehicleLat, lon: item.vehicleLon}) state.data.routePts.points of
                Mb.Just rotation -> rotation
                Mb.Nothing -> vehicleRotationCalculation item state
            wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
        locationResp <- EHC.liftFlow $ JB.isCoordOnPath state.data.routePts item.vehicleLat item.vehicleLon 1
        markerAvailable <- EHC.liftFlow $ runEffectFn1 JB.checkMarkerAvailable item.vehicleId
        if (markerAvailable)
          then 
            EHC.liftFlow $ runEffectFn1 JB.updateMarkersOnRoute
              JB.updateMarkerOnRouteConfig
                { currentVehicleLocation = { lat: item.vehicleLat, lng: item.vehicleLon }
                , pureScriptID = (EHC.getNewIDWithTag "BusTrackingScreenMap")
                , srcMarker = markerConfig {position = { lat: item.vehicleLat, lng: item.vehicleLon}}
                , srcHeaderArrowMarker = srcHeaderArrowMarkerConfig {position = { lat: item.vehicleLat, lng: item.vehicleLon}}
                , vehicleRotationFromPrevLatLon = if item.nearestWaypointConfig.deviationDistance < wmbFlowConfig.maxDeviatedDistanceInMeters then vehicleRotationFromPrevLatLon else (-1.0)
                }
          else do
            void $ EHC.liftFlow $ JB.showMarker markerConfig item.vehicleLat item.vehicleLon 70 0.5 0.5 (EHC.getNewIDWithTag "BusTrackingScreenMap")
            void $ EHC.liftFlow $ JB.showMarker srcHeaderArrowMarkerConfig {rotation = vehicleRotationFromPrevLatLon} item.vehicleLat item.vehicleLon 105 0.5 0.5 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  pure unit

vehicleRotationCalculation :: ST.VehicleData -> ST.BusTrackingScreenState -> Number
vehicleRotationCalculation item state =
  let routePts = state.data.routePts.points
  in case DA.index routePts (item.nearestWaypointConfig.index + 1), DA.index routePts (item.nearestWaypointConfig.index - 1) of
    Mb.Just nextLatLong, _ -> rotationBetweenLatLons (API.LatLong { lat: item.vehicleLat, lon: item.vehicleLon }) (API.LatLong { lat: nextLatLong.lat, lon: nextLatLong.lng })
    _, Mb.Just prevLatLong  -> rotationBetweenLatLons (API.LatLong { lat: prevLatLong.lat, lon: prevLatLong.lng }) (API.LatLong { lat: item.vehicleLat, lon: item.vehicleLon })
    _, _ ->
      rotationBetweenLatLons (API.LatLong { lat: item.vehicleLat, lon: item.vehicleLon }) $ (API.LatLong {lat: item.nextStopLat, lon: item.nextStopLon})
          -- let mbNextStopData= DA.find (\(API.VehicleInfo vehicleInfo) ->
          --           let (API.RouteStopMapping nextStop) = vehicleInfo.nextStop
          --           in nextStop.stopCode == item.nextStop
          --         ) resp.vehicleTrackingInfo
          -- case mbNextStopData of
            -- Mb.Just (API.VehicleInfo nextStopData) -> 
              -- let (API.RouteStopMapping mbNextStop) = nextStopData.nextStop 
            -- Mb.Nothing -> 0.0

userBoardedActions :: ST.BusTrackingScreenState -> Array ST.VehicleData -> ST.VehicleData -> Flow GlobalState Unit 
userBoardedActions state vehicles vehicle = do
  for_ vehicles
    $ \(item) -> do
        if item.vehicleId /= vehicle.vehicleId then do
          let _ = JB.removeMarker item.vehicleId
              _ = JB.removeMarker $ item.vehicleId <> "arrow_marker"
          pure unit
        else pure unit
  locationResp <- EHC.liftFlow $ JB.isCoordOnPath ({points : state.data.routePts.points}) (vehicle.vehicleLat) (vehicle.vehicleLon) 1
  -- locationResp <- EHC.liftFlow $ JB.isCoordOnPath state.data.routePts (vehicle.vehicleLat) (vehicle.vehicleLon) 1
  let routeConfig = JB.mkRouteConfig { points: locationResp.points } JB.defaultMarkerConfig JB.defaultMarkerConfig Mb.Nothing "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig 
  let srcMarkerConfig = JB.defaultMarkerConfig { markerId = vehicle.vehicleId, pointerIcon = "ny_ic_bus_nav_on_map" , markerSize = 70.0,  zIndex = 0.1}
      srcHeaderArrowMarkerConfig = JB.defaultMarkerConfig { markerId = vehicle.vehicleId <> "arrow_marker", pointerIcon = "ny_ic_nav_on_map_yellow_arrow" , markerSize = 105.0, zIndex = 0.0}
      vehicleRotationFromPrevLatLon = 
        case calculateVehicleBearingViaRoute (API.LatLong {lat: vehicle.vehicleLat, lon: vehicle.vehicleLon}) state.data.routePts.points of
          Mb.Just rotation -> rotation
          Mb.Nothing -> vehicleRotationCalculation vehicle state
      srcCode = Mb.maybe "" (_.stationCode) state.data.sourceStation
      destinationCode = Mb.maybe "" (_.stationCode) state.data.destinationStation
      destinationStation = DA.find (\(API.FRFSStationAPI item) -> item.code == destinationCode) state.data.stopsList
      wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
  -- filteredRoute <- case destinationStation, DS.null state.data.bookingId of
  --   Mb.Just (API.FRFSStationAPI dest), false -> do
  --     -- locationResp <- EHC.liftFlow $ JB.isCoordOnPath state.data.routePts (Mb.fromMaybe 0.0 vehicle.vehicleLat) (Mb.fromMaybe 0.0 vehicle.vehicleLon) 1
  --     locationResp <- EHC.liftFlow $ JB.isCoordOnPath ({points : DA.reverse state.data.routePts.points}) (vehicle.vehicleLat) (vehicle.vehicleLon) 1
  --     pure $ if locationResp.isInPath then { points: DA.reverse locationResp.points } else state.data.routePts
  --   _, _ -> pure state.data.routePts
  markerAvailable <- EHC.liftFlow $ runEffectFn1 JB.checkMarkerAvailable vehicle.vehicleId
  EHC.liftFlow $ JB.drawRoute [ routeConfig {routeWidth = 10} ] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  if (markerAvailable)
    then 
      EHC.liftFlow $ runEffectFn1 JB.updateMarkersOnRoute
        JB.updateMarkerOnRouteConfig
          { currentVehicleLocation = { lat: vehicle.vehicleLat, lng: vehicle.vehicleLon }
          , pureScriptID = (EHC.getNewIDWithTag "BusTrackingScreenMap")
          , srcMarker = srcMarkerConfig {position = { lat: vehicle.vehicleLat, lng: vehicle.vehicleLon} }--, eta = metersToKm locationResp.distance false}
          , srcHeaderArrowMarker = srcHeaderArrowMarkerConfig {position = { lat: vehicle.vehicleLat, lng: vehicle.vehicleLon}}
          , vehicleRotationFromPrevLatLon = if vehicle.nearestWaypointConfig.deviationDistance < wmbFlowConfig.maxDeviatedDistanceInMeters then vehicleRotationFromPrevLatLon else (-1.0)
          }
    else do
      void $ EHC.liftFlow $ JB.showMarker srcMarkerConfig vehicle.vehicleLat vehicle.vehicleLon 70 0.5 0.5 (EHC.getNewIDWithTag "BusTrackingScreenMap")
      void $ EHC.liftFlow $ JB.showMarker srcHeaderArrowMarkerConfig {rotation = vehicleRotationFromPrevLatLon} vehicle.vehicleLat vehicle.vehicleLon 105 0.5 0.5 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  EHC.liftFlow $ JB.animateCamera vehicle.vehicleLat vehicle.vehicleLon 15.0 "ZOOM"
  
  

  -- let markerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId, pointerIcon = "ny_ic_bus_nav_on_map" }
  -- void $ EHC.liftFlow $ JB.showMarker markerConfig vehicle.vehicleLat vehicle.vehicleLon 160 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  pure unit

-- metersToKm :: Int -> Boolean -> String
-- metersToKm distance towardsDrop =
--   if (distance <= 10) then
--     (if towardsDrop then (getString AT_DROP) else (getString AT_PICKUP))
--   else if (distance < 1000) then (HU.toStringJSON distance <> " m " <> (getString AWAY_C)) else (HU.parseFloat ((DI.toNumber distance) / 1000.0)) 2 <> " km " <> (getString AWAY_C)

calculateMinETADistance :: Array ST.VehicleData -> Mb.Maybe Int
calculateMinETADistance trackingData =
  minimum $ DA.mapMaybe (\item -> item.etaDistance <#> DI.floor) trackingData


calculateMinEtaTimeWithDelay :: Array ST.VehicleData -> Tuple (Mb.Maybe Int) (Mb.Maybe String)
calculateMinEtaTimeWithDelay trackingData =
  let minEta = minimum $ DA.mapMaybe (\item -> item.eta) trackingData
      timestamp = getMinEtaTimestamp minEta trackingData
  in Tuple minEta timestamp

getMinEtaTimestamp :: Mb.Maybe Int -> Array ST.VehicleData -> Mb.Maybe String
getMinEtaTimestamp minEta trackingData =
  _.timestamp <$> DA.find (\item -> item.eta == minEta) trackingData

isPostBookingTracking :: ST.BusTrackingScreenState -> Boolean
isPostBookingTracking state = 
  not $ DS.null state.data.bookingId