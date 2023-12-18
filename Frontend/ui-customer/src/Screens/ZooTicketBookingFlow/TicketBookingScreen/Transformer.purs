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

import Accessor (_ticketShortId, _ticketPlaceId, _ticketPlaceName, _personId, _amount, _visitDate, _status, _services)
import Data.Array (length, mapWithIndex, (!!), filter)
import Data.String (toUpper)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Services.API as API
import Data.Lens((^.))

import Screens.Types(TicketBookingItem(..),IndividualBookingItem(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..), TicketBookingPeopleCategoryDetails(..))

buildBookingDetails :: (Array API.TicketBookingAPIEntity) -> Array TicketBookingItem
buildBookingDetails res =
  map (\(API.TicketBookingAPIEntity item) -> do
      {
        shortId : item.ticketShortId,
        ticketPlaceId : item.ticketPlaceId ,
        ticketPlaceName : item.ticketPlaceName,
        personId : item.personId,
        amount : item.amount,
        visitDate : item.visitDate,
        status : (getBookingStatus item.status)
      }
  ) (res)

ticketDetailsTransformer :: API.TicketBookingDetails-> IndividualBookingItem
ticketDetailsTransformer resp = 
  { 
    shortId : (resp ^. _ticketShortId),
    ticketPlaceId : (resp ^. _ticketPlaceId) ,
    ticketPlaceName :( resp ^. _ticketPlaceName),
    personId : (resp ^. _personId),
    amount : (resp ^. _amount),
    visitDate : (resp ^. _visitDate),
    status : (getBookingStatus (resp ^. _status)),
    services : (ticketServicesTransformer (resp ^. _services) )
  }

ticketServicesTransformer :: (Array API.TicketBookingServiceDetails) -> (Array TicketBookingServiceDetails)
ticketServicesTransformer services = 
  map (\(API.TicketBookingServiceDetails item) -> do
      {
        ticketServiceShortId : item.ticketServiceShortId,
        ticketServiceName : item.ticketServiceName,
        amount : item.amount,
        status : item.status,
        verificationCount : item.verificationCount,
        expiryDate : item.expiryDate,
        categories : ticketBookingCategoriesTransformer item.categories,
        slot : item.slot
      }
  ) (services)

ticketBookingCategoriesTransformer :: (Array API.TicketBookingCategoryDetails) -> (Array TicketBookingCategoryDetails)
ticketBookingCategoriesTransformer categories =
  map (\(API.TicketBookingCategoryDetails item) -> do
      {
        amount : item.amount,
        bookedSeats : item.bookedSeats,
        name : item.name,
        peopleCategories : ticketBookingPCTransformer item.peopleCategories
      }
  ) (categories)

ticketBookingPCTransformer :: (Array API.TicketBookingPeopleCategoryDetails) -> (Array TicketBookingPeopleCategoryDetails)
ticketBookingPCTransformer pcs = 
  map (\(API.TicketBookingPeopleCategoryDetails item) -> do
      {
        name : item.name,
        numberOfUnits : item.numberOfUnits,
        pricePerUnit : item.pricePerUnit
      }
  ) (pcs)

dummyTicketBookingApiEntity :: API.TicketBookingAPIEntity
dummyTicketBookingApiEntity = API.TicketBookingAPIEntity {
  ticketShortId : "",
  ticketPlaceId : "" ,
  ticketPlaceName : "",
  personId : "" ,
  amount : 0.0,
  visitDate : "2016-07-22",
  status : (show API.Pending)
}

getBookingStatus status = 
  case (toUpper status )of 
    "PENDING" -> (API.Pending)
    "FAILED" -> API.Failed
    "BOOKED" -> API.Booked 
    _ -> API.Pending