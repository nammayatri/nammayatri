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
import Data.Traversable (traverse, for_)
import JBridge as JB
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
import Accessor (_lat, _lon)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Constants.Configs (getPolylineAnimationConfig)
import Screens.TicketBookingFlow.BusTrackingScreen.Transformer
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Either
import Data.Map as DM
import Data.Newtype (unwrap)
import Data.Tuple as DT
import Data.Foldable (foldM)
import Engineering.Helpers.RippleCircles as EHR
import Effect.Uncurried (runEffectFn1)
import Foreign.Class (encode)
import Storage (setValueToLocalStore)
import Data.Argonaut.Core as AC
import DecodeUtil as DU
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import Control.Alt ((<|>))
import Common.RemoteConfig.Utils as RU
import Common.Types.App as CT

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data ScreenOutput
  = Exit ST.BusTrackingScreenState
  | GoToBusTicketBooking ST.BusTrackingScreenState
  | GoToSearchLocation ST.BusTrackingScreenState
  | GoToViewTicket ST.BusTrackingScreenState

data Action
  = AfterRender
  | BackPressed
  | CurrentLocationCallBack String String String
  | UpdateTracking (Array ST.VehicleData)
  | MapReady String String String
  | NoAction
  | BookTicketButtonAction PrimaryButton.Action
  | ToggleStops
  | UpdateStops API.GetMetroStationResponse
  | ViewTicket
  | UserBoarded
  | SaveRoute JB.Locations

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

eval (UpdateStops (API.GetMetroStationResponse metroResponse)) state = do
  let
    filteredResp = case state.data.destinationStation, DS.null state.data.bookingId of
      Mb.Just dest, false -> do
        case DA.findIndex (\(API.FRFSStationAPI x) -> x.code == dest.stationCode) metroResponse of
          Mb.Nothing -> metroResponse -- If the item isn't found, return the original array
          Mb.Just index -> DA.take (index + 1) metroResponse
      _, _ -> metroResponse
    
    destinationStationSeq = Mb.maybe Mb.Nothing (\(API.FRFSStationAPI dest) -> dest.sequenceNum)(DA.last filteredResp)

    finalMap =
      DA.foldl
        ( \acc item@(API.FRFSStationAPI station) -> case acc.previousStop of
            Mb.Nothing -> acc { previousStop = Mb.Just item }
            Mb.Just stop -> acc { outputMap = DM.insert station.code stop acc.outputMap, previousStop = Mb.Just item }
        )
        { outputMap: DM.empty, previousStop: Mb.Nothing }
        filteredResp
  continueWithCmd state { data { stopsList = filteredResp, previousStopsMap = finalMap.outputMap, stationResponse = Mb.Just metroResponse }, props { showShimmer = false, destinationSequenceNumber = destinationStationSeq } }
    [ do
        void $ launchAff $ EHC.flowRunner defaultGlobalState
          $ do
              when state.props.gotMapReady $ drawDriverRoute metroResponse state
              -- drawDriverRouteMock mockRoute
              
              pure unit
        pure NoAction
    ]

eval (BookTicketButtonAction PrimaryButton.OnClick) state = exit $ GoToSearchLocation state

eval BackPressed state = exit $ GoToBusTicketBooking state

eval ViewTicket state = exit $ GoToViewTicket state

eval (UpdateTracking trackingData) state = do
  case state.props.vehicleTrackingId of
    Mb.Just id -> do
      let vehicleDetails = DA.find (\item -> item.vehicleId == id) trackingData
      case vehicleDetails of 
        Mb.Just details -> do
          continueWithCmd state [do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ userBoardedActions state trackingData details
            pure NoAction
          ]
        Mb.Nothing -> processTrackingData
    Mb.Nothing -> processTrackingData
  where
    processTrackingData = do
      let
        alreadyOnboardedThisBus = DA.find (\busInfo -> busInfo.bookingId == state.data.bookingId) extractBusOnboardingInfo
        finalMap =
          DA.foldl
            ( \acc item -> do
                let alreadyOnboardedThisVehicleInRouteOrPreTracking = Mb.isNothing alreadyOnboardedThisBus || (alreadyOnboardedThisBus <#> _.vehicleId) == Mb.Just item.vehicleId
                    -- _ = spy "codex-coming-here" $ show $ filterVehicleInfoLogic item
                if ((Mb.maybe false (_ < item.nextStopSequence) state.props.destinationSequenceNumber && DS.null state.data.bookingId) || (filterVehicleInfoLogic item.timestamp))
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
            [ pure UserBoarded ]
        Mb.Nothing -> 
          continueWithCmd state
            { data { vehicleTrackingData = DT.fst finalMap, vehicleData = trackingData }
            , props
              { busNearSourceData = DT.snd finalMap
              }
            }
          [ do
              case DT.snd finalMap of
                Mb.Just pt -> do
                  void $ JB.animateCamera pt.vehicleLat pt.vehicleLon 17.0 "ZOOM"
                Mb.Nothing -> pure unit
              pure NoAction
          ]
    

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

eval UserBoarded state = do
  let filterStopsIndex = DA.findIndex (\(API.FRFSStationAPI item) -> item.code == sourceCode) state.data.stopsList
      sourceCode = Mb.fromMaybe "" $ state.data.sourceStation <#> _.stationCode
      filteredStops = case filterStopsIndex of 
        Mb.Just index -> DA.slice index (DA.length state.data.stopsList) state.data.stopsList
        Mb.Nothing -> state.data.stopsList
      -- findBusInfo = DA.find (\busInfo -> busInfo.bookingId == state.data.bookingId) extractBusOnboardingInfo
      vId = (state.props.busNearSourceData <#> _.vehicleId) <|> state.props.vehicleTrackingId
  when (Mb.isJust vId && state.data.bookingId /= "" && Mb.isNothing state.props.vehicleTrackingId) do
    void $ pure $ setValueToCache (show ONBOARDED_VEHICLE_INFO) updatedOnboardingVehicleInfo (DU.stringifyJSON <<< encode)
  continueWithCmd state {props{individualBusTracking = true, vehicleTrackingId = vId, expandStopsView = if (Mb.isNothing state.props.vehicleTrackingId) then false else state.props.expandStopsView}, data{stopsList = filteredStops}} [ do
    -- void $ launchAff $ EHC.flowRunner defaultGlobalState $ userBoardedActions state
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

eval _ state = update state

userBoardedActions :: ST.BusTrackingScreenState -> Array ST.VehicleData -> ST.VehicleData -> Flow GlobalState Unit 
userBoardedActions state vehicles vehicle = do
  for_ vehicles
    $ \(item) -> do
        if item.vehicleId /= vehicle.vehicleId then do
          let _ = JB.removeMarker item.vehicleId
          pure unit
        else pure unit
  locationResp <- EHC.liftFlow $ JB.isCoordOnPath state.data.routePts (vehicle.vehicleLat) (vehicle.vehicleLon) 1
  let routeConfig = JB.mkRouteConfig { points: locationResp.points } JB.defaultMarkerConfig JB.defaultMarkerConfig Mb.Nothing "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig 
  EHC.liftFlow $ JB.drawRoute [ routeConfig{routeColor = Color.black800} ] (EHC.getNewIDWithTag "BusTrackingScreenMap")

  -- let markerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId, pointerIcon = "ny_ic_bus_marker" }
  -- void $ EHC.liftFlow $ JB.showMarker markerConfig vehicle.vehicleLat vehicle.vehicleLon 160 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
  pure unit

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
            Right (API.FRFSRouteAPI routeResp) -> DA.reverse $ Mb.fromMaybe [] routeResp.waypoints
            Left err -> []
    _ = spy "codex-response" response
    _ = spy "codex-route" route

    destinationStation = DA.find (\(API.FRFSStationAPI item) -> item.code == destinationCode) resp
  filteredRoute <- case destinationStation, DS.null state.data.bookingId of
    Mb.Just (API.FRFSStationAPI dest), false -> do
      locationResp <- EHC.liftFlow $ JB.isCoordOnPath route (Mb.fromMaybe 0.0 dest.lat) (Mb.fromMaybe 0.0 dest.lon) 1
      pure $ if locationResp.isInPath then { points: locationResp.points } else route
    _, _ -> pure route
  push <- EHC.liftFlow $ getPushFn Mb.Nothing "BusTrackingScreen"
  EHC.liftFlow $ push $ SaveRoute filteredRoute
  let
    routeConfig = transformStationsForMap resp filteredRoute srcCode destinationCode
  void $ pure $ JB.removeAllPolylines ""
  void $ pure $ JB.removeAllMarkers ""
  EHC.liftFlow $ JB.drawRoute [ routeConfig ] (EHC.getNewIDWithTag "BusTrackingScreenMap")
  EHC.liftFlow $ JB.setMapPadding 0 0 0 300
  void $ foldM processStop 0 state.data.stopsList
  where
  processStop index (API.FRFSStationAPI item) = do
    let
      lat = Mb.fromMaybe 0.0 item.lat
      lon = Mb.fromMaybe 0.0 item.lon
      markerId = item.code
      pointertype = getStopType item.code index state
      size = getStopMarkerSize pointertype
    void $ EHC.liftFlow $ JB.showMarker JB.defaultMarkerConfig { markerId = item.code, pointerIcon = getStopMarker pointertype, primaryText = item.name } lat lon size 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
    when (DA.elem pointertype [SOURCE_STOP, DESTINATION_STOP]) $ EHC.liftFlow $ runEffectFn1 
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

extractBusOnboardingInfo :: ST.OnboardedBusInfo
extractBusOnboardingInfo = 
  Mb.fromMaybe [] decodedOnboardedBusInfo
  where
    decodedOnboardedBusInfo :: Mb.Maybe ST.OnboardedBusInfo
    decodedOnboardedBusInfo = 
      let cachedOnboardedBusInfo = JB.getKeyInSharedPrefKeys $ show ONBOARDED_VEHICLE_INFO
      in DU.decodeForeignAny (DU.parseJSON cachedOnboardedBusInfo) Mb.Nothing

-- Filtering Vehicle Info based on external logic based on remote config
filterVehicleInfoLogic :: String -> Boolean
filterVehicleInfoLogic timestamp =
  let timeDiff = EHC.compareUTCDate (EHC.getCurrentUTC "") timestamp 
      wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
  in (timeDiff > wmbFlowConfig.maxAllowedTimeDiffInLTSinSec)
      -- && (item.nextStopDistance > wmbFlowConfig.maxDeviatedDistanceInMeters) -- Todo - check via haversine polyline distance calculation

