

module Screens.TicketBookingFlow.MetroMyTickets.Transformer where


import Prelude 
import Screens.Types 
import Services.API
import Data.Array
import Data.Maybe


metroTicketListApiToMyTicketsTransformer ::  (Array MetroTicketBookingStatus) -> MetroMyTicketsScreenState -> MetroMyTicketsScreenState 
metroTicketListApiToMyTicketsTransformer ticketList state = 
  let 
    activeTickets' = metroTicketCardTransformer $ filter (\ (MetroTicketBookingStatus ticket) -> (ticket.status == "CONFIRMED")) ticketList
    pastTickets' = metroTicketCardTransformer $ filter (\ (MetroTicketBookingStatus ticket) -> (ticket.status /= "CONFIRMED")) ticketList
  in
    state {
      data {
        activeTickets =  activeTickets'
      , pastTickets = pastTickets'
      }
    }

metroTicketCardTransformer :: Array MetroTicketBookingStatus -> Array MetroTicketCardData
metroTicketCardTransformer  ticketList = map ticketItemTransformer ticketList

ticketItemTransformer :: MetroTicketBookingStatus -> MetroTicketCardData
ticketItemTransformer (MetroTicketBookingStatus bookingItem) = 
  let 
    sourceStationEnum = bookingItem.stations !! 0
    destinationStationEnum = bookingItem.stations !! ((length bookingItem.stations)- 1)
    
    sourceName' = getStationName sourceStationEnum
    destinationName' = getStationName destinationStationEnum
    noOfTickets' = length bookingItem.tickets
    createdAt' = ""
    metroTicketStatusApiResp' = (MetroTicketBookingStatus bookingItem)
    status' = bookingItem.status
    validUntill' = bookingItem.validTill
  in 
  {
      sourceName : sourceName'
    , destinationName : destinationName'
    , createdAt : createdAt'
    , noOfTickets : noOfTickets'
    , metroTicketStatusApiResp :  metroTicketStatusApiResp'
    , status : status'
    , validUntill : validUntill'
  }

getStationName :: Maybe FRFSStationAPI -> String
getStationName Nothing = ""
getStationName (Just (FRFSStationAPI station)) = station.name 
