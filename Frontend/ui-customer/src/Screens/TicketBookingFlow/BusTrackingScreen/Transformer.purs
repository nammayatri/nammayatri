{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.BusTrackingScreen.Transformer where


import Prelude 
import Screens.Types 
import Services.API as API
import Data.Array as DA
import Data.Maybe
import Engineering.Helpers.Commons
import Data.Function.Uncurried as DFU
import Helpers.Utils as HU
import JBridge as JB
import Services.Backend as Remote 
import Constants.Configs (getPolylineAnimationConfig)
import Debug
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData (StopType (..))
import MapUtils (haversineDistance)
import Data.Tuple (Tuple(..))
import Data.Foldable (minimumBy)

transformStationsForMap :: Array API.FRFSStationAPI -> JB.Locations -> String -> String -> JB.RouteConfig
transformStationsForMap stations route srcCode destCode = do
  let res = DA.foldl (\acc word -> if acc.index == 0 then acc{src = Just word, index = acc.index + 1}
                      else if acc.index == (DA.length stations -1)  then acc{dest = Just word, index = acc.index + 1}
                      else acc{stops = acc.stops <> [word], index = acc.index + 1} ) {src : Nothing, dest : Nothing, stops : [], index : 0} stations
  case res.src, res.dest of
    Just (API.FRFSStationAPI s), Just (API.FRFSStationAPI d) -> do
      let stops = res.stops
          markers = HU.normalRoute ""
          len = DA.length stops
          sourcePosition = {lat : fromMaybe 0.0 s.lat, lng :fromMaybe 0.0  s.lon}
          destPosition = {lat : fromMaybe 0.0 d.lat, lng :fromMaybe 0.0 d.lon}
          srcMarkerConfig = JB.defaultMarkerConfig  { markerId = s.code, pointerIcon = getMarkerImage s.code (-1) len, primaryText = s.name, position = sourcePosition, markerSize = if getMarkerImage s.code (-1) len == "ny_ic_stop_black" then 20.0 else 90.0, useMarkerSize = true}
          destMarkerConfig = JB.defaultMarkerConfig { markerId = d.code, pointerIcon = getMarkerImage d.code len len, primaryText = d.name, position = destPosition, markerSize = if getMarkerImage d.code (len) len == "ny_ic_stop_black" then 20.0 else 90.0, useMarkerSize = true}
          stopsConfig = DA.mapWithIndex (\index (API.FRFSStationAPI item) -> JB.defaultMarkerConfig{ markerId = item.code, pointerIcon = getMarkerImage item.code index len, position = {lat : fromMaybe 0.0 item.lat, lng : fromMaybe 0.0  item.lon}, markerSize = 10.0, useMarkerSize = true}) stops
      JB.mkRouteConfig route JB.defaultMarkerConfig JB.defaultMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig 
    _,_ -> JB.routeConfig
    where 
      getMarkerImage code index ln = if srcCode /= "" && srcCode == code then markers.srcMarker 
                            else if destCode /= "" && destCode == code then markers.destMarker
                            else if srcCode /= "" && destCode /= "" then "ny_ic_stop_black"
                            else if index == -1 then markers.srcMarker 
                            else if index == ln  then markers.destMarker
                            else "ny_ic_stop_black"
      markers = HU.normalRoute ""

getStationsFromBusRoute ::  API.FRFSRouteAPI -> Array API.FRFSStationAPI 
getStationsFromBusRoute (API.FRFSRouteAPI stop) = fromMaybe [] stop.stations

getStopImage :: StopType -> Maybe RideType -> String 
getStopImage stopType rideType = 
  case stopType, rideType of
    SOURCE_STOP, Just STOP -> "ny_ic_source_dot"
    SOURCE_STOP, _ -> "ny_ic_stop_black"
    DESTINATION_STOP, Just STOP -> "ny_ic_dest_dot"
    DESTINATION_STOP, _ -> "ny_ic_stop_black"
    ROUTE_SOURCE, _ -> "ny_ic_stop_black"
    ROUTE_END, _ -> "ny_ic_stop_black"
    _, _ -> "ny_ic_stop_black"

getStopMarker :: StopType -> Maybe RideType -> Int -> String 
getStopMarker stopType rideType stopIndex = do
  let markers = HU.normalRoute ""
  case stopType,rideType of
    SOURCE_STOP, Just STOP -> markers.srcMarker
    SOURCE_STOP, _ -> if stopIndex == 0 then "ny_ic_route_start_pointer" else markers.srcMarker
    DESTINATION_STOP, Just STOP -> markers.destMarker
    DESTINATION_STOP, _ -> "ny_ic_route_end_pointer"
    ROUTE_SOURCE, _ -> "ny_ic_route_start_pointer"
    ROUTE_END, _ -> "ny_ic_route_end_pointer"
    _, _ -> "ny_ic_stop_black"

getStopMarkerSize :: StopType -> Maybe RideType -> Int -> Int 
getStopMarkerSize stopType rideType stopIndex = 
  case stopType, rideType of
    SOURCE_STOP, Just STOP -> 90
    SOURCE_STOP, _ -> if stopIndex == 0 then 120 else 90
    DESTINATION_STOP, Just STOP -> 90
    DESTINATION_STOP, _ -> 120
    NORMAL_STOP, _ -> 20
    ROUTE_SOURCE, _ -> 120
    ROUTE_END, _ -> 120
    _, _ -> 50

getZIndex :: StopType -> Number
getZIndex stopType = do
  case stopType of
    SOURCE_STOP      -> 1.0
    DESTINATION_STOP -> 1.0
    _                -> 0.0

getStopType :: String -> Int -> BusTrackingScreenState-> StopType 
getStopType code index state = do
  let srcCode = maybe (state.data.nearestStopFromCurrentLoc <#> (\(API.FRFSStationAPI nearestStop) -> nearestStop.code)) (\item -> Just $ item.stationCode) state.data.sourceStation
      destCode = state.data.destinationStation <#> _.stationCode
  if isJust srcCode  && srcCode == Just code then SOURCE_STOP
  else if isJust destCode  && destCode == Just code then DESTINATION_STOP
  else if isJust srcCode  &&  index == 0 then ROUTE_SOURCE
  else if isJust srcCode  &&  index == DA.length state.data.stopsList - 1 then ROUTE_END
  else if index == 0 then SOURCE_STOP
  else if index == DA.length state.data.stopsList - 1 then DESTINATION_STOP
  else NORMAL_STOP

getNearestStopFromLatLong :: API.LatLong -> Array API.FRFSStationAPI -> Maybe API.FRFSStationAPI
getNearestStopFromLatLong currentLocation stations =
  let minDistance = minimumBy (\(Tuple _ d1) (Tuple _ d2) -> compare d1 d2) distances
  in 
    case minDistance of
      Just (Tuple nearestStop _) -> Just nearestStop
      Nothing -> Nothing
  where
    distances :: Array (Tuple API.FRFSStationAPI Number)
    distances = map (\(API.FRFSStationAPI item) -> Tuple (API.FRFSStationAPI item) $ haversineDistance currentLocation (API.LatLong {lat : fromMaybe 0.0 item.lat, lon : fromMaybe 0.0 item.lon})) stations