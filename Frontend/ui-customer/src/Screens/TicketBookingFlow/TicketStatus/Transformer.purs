{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.TicketStatus.Transformer where

import Prelude

import Data.Array
import Data.String
import Screens.Types


metroTicketDetailsTransformer :: Array KeyVal 
metroTicketDetailsTransformer = [
  {key : "Date", val : "22-07-2016"}
, {key : "No of Tickets", val : "6"}
, {key : "Total Paid", val : "₹192"}
, {key : "Booking ID", val : "-"}
, {key : "Transaction ID", val : "OR8192BY29213292149824UI"}
, {key : "Valid until", val : "5 PM, 10th Nov 2023"}
]