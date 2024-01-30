{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketStatus.Transformer where

import Prelude
import Data.Array as DA
import Data.String as DS
import Screens.Types
import Services.API
import Common.Types.App as Common
import Engineering.Helpers.Commons
import Data.Maybe

metroTicketStatusTransformer :: MetroTicketBookingStatus -> String -> MetroTicketStatusScreenState -> MetroTicketStatusScreenState
metroTicketStatusTransformer (MetroTicketBookingStatus metroTicketBookingStatus) shortOrderId' state = 
  let 
    keyValArray' = metroTicketDetailsKeyVals (MetroTicketBookingStatus metroTicketBookingStatus)
    paymentStatus' = case metroTicketBookingStatus.status of 
      "CONFIRMED" -> Common.Success
      "PAYMENT_PENDING" -> Common.Pending
      "CONFIRMING" -> Common.Pending
      _ -> Common.Failed
    ticketName' = "Tickets for Chennai Metro"
    validUntil' =  metroTicketBookingStatus.validTill
  in
    state {
      data{
        ticketName = ticketName'
      , keyValArray = keyValArray'
      , shortOrderId = shortOrderId'
      , validUntil = validUntil'
      }
    , props{
        paymentStatus =  paymentStatus'
      }
    }


metroTicketDetailsKeyVals ::  MetroTicketBookingStatus -> Array KeyVal 
metroTicketDetailsKeyVals (MetroTicketBookingStatus metroTicketBookingStatus) = 
  let 
    payment = metroTicketBookingStatus.payment
    date = convertUTCtoISC metroTicketBookingStatus.createdAt "hh:mm A, Do MMM YYYY"
    noOfTickets = show $ DA.length metroTicketBookingStatus.tickets
    totalPaid =   show $ metroTicketBookingStatus.price
    bookingId = metroTicketBookingStatus.bookingId
    paymentOrder = metroTicketBookingStatus.payment >>= (\(FRFSBookingPaymentAPI payment') ->  payment'.paymentOrder)
    transactionId = case paymentOrder of 
      Just (CreateOrderRes orderResp) -> orderResp.id
      Nothing -> ""
    validUntill = (convertUTCtoISC metroTicketBookingStatus.validTill "hh:mm A") <> ", " <> (convertUTCtoISC metroTicketBookingStatus.validTill "Do MMM YYYY")
  in
    [ {key : "Date", val : date}
    , {key : "No of Tickets", val : noOfTickets }
    , {key : "Total Paid", val : totalPaid}
    , {key : "Booking ID", val : bookingId}
    , {key : "Transaction ID", val : transactionId}
    , {key : "Valid until", val : validUntill}
    ]