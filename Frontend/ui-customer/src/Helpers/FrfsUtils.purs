{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.FrfsUtils where

import Prelude
import Screens.Types (MetroTicketCardData(..))
import Data.Array as DA
import Services.API (FRFSTicketBookingStatusAPIRes(..), FRFSTicketAPI(..), FRFSRouteAPI(..), FrfsQuote(..), FRFSBookingPaymentAPI(..), FRFSStationAPI(..), VehicleInfoForRoute(..), LatLong(..), BusTrackingRouteResp(..), UpcomingStop(..), Stop(..), VehicleInfo(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons as EHC
import Data.Int as DI
import DecodeUtil as DU
import Common.RemoteConfig.Utils as RU
import Common.Types.App as CT
import Debug (spy)

getTicketStatus :: MetroTicketCardData -> TicketStatus
getTicketStatus ticketData = 
  let (FRFSTicketBookingStatusAPIRes ticketBookingStatusResp) = ticketData.metroTicketStatusApiResp
      bookingStatus = ticketData.status
      ticketStatus = DA.head $ map (\(FRFSTicketAPI ticketD) -> ticketD.status) ticketBookingStatusResp.tickets
      paymentStatus =
        case ticketBookingStatusResp.payment of
            (Just (FRFSBookingPaymentAPI paymentInfo)) -> Just paymentInfo.status
            Nothing -> Nothing
  in
    if (bookingStatus == "FAILED") then
      if not (paymentStatus == Just "REFUND_PENDING") then
        TicketStatus { status : FRFS_FAILED, statusColor : Color.red900, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_red_triangle_warning", textColor: Color.white900}
      else
        TicketStatus { status : FRFS_PENDING, statusColor : Color.yellow900, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock", textColor: Color.white900}
    else if bookingStatus == "CONFIRMED" then
      if ticketStatus == Just "ACTIVE" then
        TicketStatus { status : FRFS_ACTIVE, statusColor : Color.green900, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", textColor: Color.white900}
      else if ticketStatus == Just "USED" then
        TicketStatus { status : FRFS_VERIFIED, statusColor : Color.blue800, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", textColor: Color.white900}
      else if ticketStatus == Nothing then
        TicketStatus { status : FRFS_PENDING, statusColor : Color.yellow900, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock", textColor: Color.white900}  
      else 
        TicketStatus { status : FRFS_EXPIRED, statusColor : Color.grey900, statusIcon : fetchImage FF_ASSET "ny_ic_info", textColor: Color.black900}
    else
      TicketStatus { status : FRFS_PENDING, statusColor : Color.yellow900, statusIcon : fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock", textColor: Color.white900}

getFirstRoute :: FrfsQuote -> Maybe FRFSRouteAPI
getFirstRoute (FrfsQuote quote) =
  case quote.routeStations of
    Just routes ->
      case routes DA.!! 0 of
        Just route -> Just route
        Nothing -> Nothing
    Nothing -> Nothing

getAllFirstRoutes :: Maybe (Array FrfsQuote) -> Array FRFSRouteAPI
getAllFirstRoutes maybeQuotes =
  DA.catMaybes $
    case maybeQuotes of
      Just quotes ->
        map (\quote ->
              getFirstRoute quote
            ) quotes
      Nothing -> []

getSortedStops :: Array FRFSStationAPI -> Array FRFSStationAPI
getSortedStops stops =
  let
    maxBound = top
    distanceValue :: FRFSStationAPI -> Int
    distanceValue (FRFSStationAPI { distance }) =
      fromMaybe maxBound distance
  in
    DA.sortBy (comparing distanceValue) stops

newtype TicketStatus = TicketStatus 
  { status :: FRFSTicketStatus
  , statusColor :: String
  , statusIcon :: String
  , textColor :: String
  }

data FRFSTicketStatus = FRFS_EXPIRED 
                      | FRFS_VERIFIED 
                      | FRFS_ACTIVE 
                      | FRFS_FAILED 
                      | FRFS_PENDING 
                      | FRFS_CANCELLED 
                      | FRFS_CANCELLATION_PENDING 
                      | FRFS_CANCELLATION_FAILED

derive instance genericFRFSTicketStatus :: Generic FRFSTicketStatus _ 
instance showFRFSTicketStatus :: Show FRFSTicketStatus where
  show FRFS_EXPIRED = "Expired"
  show FRFS_VERIFIED = "Verified"
  show FRFS_ACTIVE = "Active"
  show FRFS_FAILED = "Failed"
  show FRFS_PENDING = "Pending"
  show FRFS_CANCELLED = "Cancelled"
  show FRFS_CANCELLATION_PENDING = "Cancellation Pending"
  show FRFS_CANCELLATION_FAILED = "Cancellation Failed"
instance eqFRFSTicketStatus :: Eq FRFSTicketStatus where eq = genericEq


calculateEtaForBusRoutes :: VehicleInfo -> String -> Array (Maybe Int)
calculateEtaForBusRoutes (VehicleInfo vehicleData) srcLoc = do
    let 
      (VehicleInfoForRoute vehicleInfoForRoute) = vehicleData.vehicleInfo
      lat = fromMaybe 0.0 vehicleInfoForRoute.latitude
      lon = fromMaybe 0.0 vehicleInfoForRoute.longitude
      wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
    case filterVehicleInfoLogic (VehicleInfo vehicleData) wmbFlowConfig of
      false -> []
      _ ->
        let 
          upcomgingStops = fromMaybe [] vehicleInfoForRoute.upcomingStops
          etaTillPickupStop = DA.find (\(UpcomingStop upcomingStop) ->
              let (Stop stop) = upcomingStop.stop
                  _ = spy "Upcoming Stop" upcomingStop.stop
              in (srcLoc ==  stop.name)
            ) upcomgingStops 
          lastReachedStop = DA.find (\(UpcomingStop upcomingStop) -> upcomingStop.status == "Reached") $ DA.reverse upcomgingStops 
          firstUpcomingStop = DA.find (\(UpcomingStop upcomingStop) -> upcomingStop.status == "Upcoming") upcomgingStops
          upcomingNextStop = firstUpcomingStop <#> (\(UpcomingStop upcomingStop) -> upcomingStop.stop)
          nextStopLatLon = fromMaybe (LatLong {lat: 0.0, lon: 0.0}) $ upcomingNextStop <#> (\(Stop stop) -> stop.coordinate) -- (API.LatLong nextStopPosition)
          nextStopIdx = fromMaybe (0) $ upcomingNextStop <#> (\(Stop stop) -> stop.stopIdx) -- nextStop.sequenceNu 
        in 
          [filterETAForOlderLatLong vehicleInfoForRoute.timestamp $ maybe Nothing (\stop -> calculateETAFromUpcomingStop lastReachedStop stop vehicleData.vehicleId) etaTillPickupStop] -- EHC.compareUTCDate  (EHC.getCurrentUTC "")
  where
    filterETAForOlderLatLong timeStamp eta' = 
      let timeDiff = EHC.compareUTCDate (EHC.getCurrentUTC "") $ timeStamp
      in if (timeDiff < 120) then eta' else Nothing

    calculateETAFromUpcomingStop lastReachedStop (UpcomingStop stop) vehicleId = 
      let etaTimeStamp = stop.eta
          delayInSeconds = DI.round $ fromMaybe 0.0 stop.delta
          etaInSeconds = EHC.compareUTCDate etaTimeStamp (EHC.getCurrentUTC "")
      in if etaInSeconds + delayInSeconds > 0
        then Just $ etaInSeconds + delayInSeconds
        else Nothing 

    filterVehicleInfoLogic (VehicleInfo vehicleData) wmbFlowConfig =
      let (VehicleInfoForRoute m) = vehicleData.vehicleInfo
          timeDiff = EHC.compareUTCDate (EHC.getCurrentUTC "") m.timestamp
      in (timeDiff < wmbFlowConfig.maxAllowedTimeDiffInLTSinSec)