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
import Language.Strings
import Language.Types
import Domain.Payments as PP


metroTicketStatusTransformer :: MetroTicketBookingStatus -> String -> MetroTicketStatusScreenState -> MetroTicketStatusScreenState
metroTicketStatusTransformer (MetroTicketBookingStatus metroTicketBookingStatus) shortOrderId' state = 
  let 
    keyValArray' = metroTicketDetailsKeyVals (MetroTicketBookingStatus metroTicketBookingStatus)
    paymentStatus' = case metroTicketBookingStatus.status of 
      "CONFIRMED" -> PP.Success
      "PAYMENT_PENDING" -> PP.Pending
      "CONFIRMING" -> PP.Pending
      _ -> PP.Failed
    ticketName' = getString TICKETS_FOR_CHENNAI_METRO
    validUntil' =  metroTicketBookingStatus.validTill
    paymentOrder = metroTicketBookingStatus.payment >>= (\(FRFSBookingPaymentAPI payment') ->  payment'.paymentOrder)
    transactionId = case paymentOrder of 
      Just (CreateOrderRes orderResp) -> orderResp.order_id
      Nothing -> ""
  in
    state {
      data{
        ticketName = ticketName'
      , keyValArray = keyValArray'
      , shortOrderId = transactionId
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
    noOfTickets = show $ metroTicketBookingStatus.quantity
    totalPaid =   show $ metroTicketBookingStatus.price
    bookingId = metroTicketBookingStatus.bookingId
    paymentOrder = metroTicketBookingStatus.payment >>= (\(FRFSBookingPaymentAPI payment') ->  payment'.paymentOrder)
    transactionId = case paymentOrder of 
      Just (CreateOrderRes orderResp) -> orderResp.order_id
      Nothing -> ""
    validUntill = (convertUTCtoISC metroTicketBookingStatus.validTill "hh:mm A") <> ", " <> (convertUTCtoISC metroTicketBookingStatus.validTill "Do MMM YYYY")
  in
    [ {key : getString DATE, val : date}
    , {key : getString NO_OF_TICKETS, val : noOfTickets }
    , {key : getString TOTAL_PAID , val : totalPaid}
    , {key : "Booking ID", val : bookingId}
    , {key : "Transaction ID", val : transactionId}
    , {key : getString VALID_UNTIL, val : validUntill}
    ]