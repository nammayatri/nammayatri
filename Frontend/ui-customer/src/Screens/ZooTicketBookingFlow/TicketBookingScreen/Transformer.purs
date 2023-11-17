{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingScreen.Transformer where

import Prelude

import Data.Array (length, mapWithIndex, (!!), filter)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Services.API as API
import Screens.Types(TicketBookingItem(..))

buildBookingDetails :: (Array API.TicketBookingAPIEntity) -> Array TicketBookingItem
buildBookingDetails res =
  map (\(API.TicketBookingAPIEntity item) -> do
      {
        shortId : item.shortId,
        ticketPlaceId : item.ticketPlaceId ,
        ticketPlaceName : item.ticketPlaceName,
        personId : item.personId,
        amount : item.amount,
        visitDate : item.visitDate,
        status : item.status
      }
  ) (res)

dummyTicketBookingApiEntity :: API.TicketBookingAPIEntity
dummyTicketBookingApiEntity = API.TicketBookingAPIEntity {
  shortId : "",
  ticketPlaceId : Nothing ,
  ticketPlaceName : "",
  personId : Nothing ,
  amount : 0.0,
  visitDate : "2016-07-22",
  status : API.Pending
}