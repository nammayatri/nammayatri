{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketDetails.Transformer where

import Prelude 
import Services.API 
import Screens.Types 
import Common.Types.App
import Data.Array
import Data.Maybe
import Data.Int as INT
import Engineering.Helpers.Commons
import Helpers.Utils (getCityNameFromCode)

metroTicketDetailsTransformer :: FRFSTicketBookingStatusAPIRes -> MetroTicketDetailsScreenState -> MetroTicketDetailsScreenState 
metroTicketDetailsTransformer (FRFSTicketBookingStatusAPIRes metroTicketBookingStatus) state = 
  let
    
    metroRoute' = metroRouteTrasformer metroTicketBookingStatus.stations
    ticketsInfo' = ticketsInfoTransformer metroTicketBookingStatus.tickets
  in 
    state {
      data {
        metroRoute = metroRoute'
      , bookingId = metroTicketBookingStatus.bookingId
      , city = getCityNameFromCode $ Just metroTicketBookingStatus.city
      , bookingUpdatedAt = metroTicketBookingStatus.updatedAt
      , ticketsInfo = ticketsInfo'
      , ticketType = metroTicketBookingStatus._type
      , noOfTickets = metroTicketBookingStatus.quantity
      , ticketPrice = metroTicketBookingStatus.price
      , vehicleType = metroTicketBookingStatus.vehicleType
      , route = metroTicketBookingStatus.routeStations
      , transactionId = extractTransactionId metroTicketBookingStatus.payment
      }
    , props {
        stage = MetroTicketDetailsStage
      , currentTicketIndex =  0
      , showLoader = false
      , isBookingCancellable = Nothing
      , cancellationCharges = Nothing
      , refundAmount = Nothing
      }
    }



--- Metro Route Transformer Logic Start------------------

metroRouteTrasformer ::  Array FRFSStationAPI -> Array MetroRoute
metroRouteTrasformer stations =
  foldl (\ acc (FRFSStationAPI station) -> acc <> (getNextInterMediates (FRFSStationAPI station) stations)) [] stations


getNextInterMediates :: FRFSStationAPI -> Array FRFSStationAPI -> Array MetroRoute
getNextInterMediates (FRFSStationAPI station) stations =
  let
    defaultRoute = {
      name: station.name,
      line: case station.color of
        Just "Green" -> GreenLine
        Just "Blue" -> BlueLine
        Just "Red" -> RedLine
        _ -> NoColorLine,
      stops: getStops (FRFSStationAPI station) stations,
      listExpanded: false
    }
  in
    case station.stationType of
      Just "INTERMEDIATE" -> []
      Just _ -> [defaultRoute]
      Nothing -> [defaultRoute]

getStops :: FRFSStationAPI -> Array FRFSStationAPI -> Array MetroStop 
getStops (FRFSStationAPI station) stations = 
  let 
    stationIndex = fromMaybe 0 (findIndex (\(FRFSStationAPI station') -> station'.name == station.name) stations) -- fromMay default won't hit never
    nextStations = drop (stationIndex + 1) stations
    endStationIndex = fromMaybe 0 (findIndex (\(FRFSStationAPI station') -> station'.stationType /= Just "INTERMEDIATE") nextStations)
    nextStations' = take endStationIndex nextStations
  in 
    map (\(FRFSStationAPI station') -> 
      {
        name : station'.name
      }) nextStations'

--- Metro Route Transformer Logic End------------------


ticketsInfoTransformer :: Array FRFSTicketAPI -> Array MetroTicketInfo
ticketsInfoTransformer tickets = 
  map (\(FRFSTicketAPI ticket) -> {
    qrString : ticket.qrData
    , ticketNumber : ticket.ticketNumber
    , validUntil : (convertUTCtoISC ticket.validTill "hh:mm A") <> ", " <> (convertUTCtoISC ticket.validTill "Do MMM YYYY") 
    , status : ticket.status
  }) tickets

extractTransactionId :: Maybe FRFSBookingPaymentAPI -> String
extractTransactionId payment =
  let dummyTransactionId = "12345678ABCD"
  in case payment of
      Just (FRFSBookingPaymentAPI paymentInfo) -> fromMaybe dummyTransactionId paymentInfo.transactionId
      Nothing -> dummyTransactionId