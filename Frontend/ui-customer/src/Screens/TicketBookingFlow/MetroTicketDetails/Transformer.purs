module Screens.TicketBookingFlow.MetroTicketDetails.Transformer where

import Prelude 
import Services.API 
import Screens.Types 
import Common.Types.App
import Data.Array
import Data.Maybe
import Engineering.Helpers.Commons

metroTicketDetailsTransformer :: MetroTicketBookingStatus -> MetroTicketDetailsScreenState -> MetroTicketDetailsScreenState 
metroTicketDetailsTransformer (MetroTicketBookingStatus metroTicketBookingStatus) state = 
  let
    
    metroRoute' = metroRouteTrasformer metroTicketBookingStatus.stations
    ticketsInfo' = ticketsInfoTransformer metroTicketBookingStatus.tickets

  in 
    state {
      data {
        metroRoute = metroRoute'
      , ticketsInfo = ticketsInfo'
      }
    , props {
        stage = MetroTicketDetailsStage
      , currentTicketIndex =  0
      }
    }



--- Metro Route Transformer Logic Start------------------

metroRouteTrasformer ::  Array FRFSStationAPI -> Array MetroRoute
metroRouteTrasformer stations =  
  foldl (\ acc (FRFSStationAPI station) -> acc <> (getNextInterMediates (FRFSStationAPI station) stations)) [] stations


getNextInterMediates :: FRFSStationAPI -> Array FRFSStationAPI -> Array MetroRoute 
getNextInterMediates (FRFSStationAPI station) stations = 
  case station.stationType of 
    Just stationType -> 
      case stationType of 
        "INTERMEDIATE" -> []
        "START" -> [{
            name : station.name
          , line : GreenLine
          , stops : getStops (FRFSStationAPI station) stations
          , listExpanded : false
          }]
        "END" -> [{
            name : station.name
          , line : RedLine
          , stops : getStops (FRFSStationAPI station) stations
          , listExpanded : false
          }]
        _ -> [{
            name : station.name
          , line : case station.color of
              Just color -> 
                case color of 
                  "Green" -> GreenLine
                  "Blue" -> BlueLine
                  "Red" -> RedLine
                  _ -> NoColorLine
              Nothing -> NoColorLine
          , stops : getStops (FRFSStationAPI station) stations
          , listExpanded : false
          }]
    Nothing -> []

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
    , status : show ticket.status
  }) tickets

