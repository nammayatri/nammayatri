{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.PlaceDetails.Transformer where

import Prelude

import Accessor (_ticketShortId, _ticketPlaceId, _ticketPlaceName, _personId, _amount, _visitDate, _status, _services)
import Data.Array (elem, find, groupBy, length, mapWithIndex, (!!), filter)
import Data.String (toUpper)
import Data.String as DS
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Services.API as API
import Data.Lens((^.))
import Screens.Types(TimeInterval, TicketCategoriesData, PeopleCategoriesRespData, BusinessHoursData, SlotsAndTimeIntervalData, TicketServiceData, TicketBookingScreenState(..), TicketBookingItem(..),IndividualBookingItem(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..), TicketBookingPeopleCategoryDetails(..))
import Data.Array.NonEmpty as DAN
import Data.Int (ceil)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC)
import Data.Function.Uncurried (runFn3)
import Helpers.Utils(incrOrDecrTimeFrom)

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


transformRespToStateData :: Boolean -> API.TicketServiceResp -> TicketBookingScreenState -> String -> TicketServiceData
transformRespToStateData isFirstElement (API.TicketServiceResp service) state selOpDay = do
  let timeIntervalDataInfo = convertServiceDataToTimeIntervalData service.businessHours
  { id : service.id,
    serviceName : service.name,
    shortDesc : service.shortDesc,
    allowFutureBooking : service.allowFutureBooking,
    expiry : service.expiry,
    businessHours : map convertServiceBusinessHoursData service.businessHours,
    timeIntervalData : timeIntervalDataInfo,
    isExpanded : isFirstElement,
    selectedBHid : getValidBHid (getValidTimeIntervals timeIntervalDataInfo selOpDay) selOpDay state.data.dateOfVisit state service.expiry,
    selectedSlot : Nothing
  }
  where
  convertServiceDataToTimeIntervalData :: Array API.BusinessHoursResp -> Array SlotsAndTimeIntervalData
  convertServiceDataToTimeIntervalData respArr = do
    let groupedBusinessHours = groupBy (\(API.BusinessHoursResp x) (API.BusinessHoursResp y) -> x.operationalDays == y.operationalDays) respArr
    map generateSlotData groupedBusinessHours

  generateSlotData :: DAN.NonEmptyArray API.BusinessHoursResp -> SlotsAndTimeIntervalData
  generateSlotData bhs = 
    let (API.BusinessHoursResp headBH) = DAN.head bhs
        businessHours = DAN.toArray bhs
    in
      { operationalDays : headBH.operationalDays,
        slot : if (isClosed headBH.specialDayType) then [] else filter (\slotinfo -> slotinfo.slot /= "") (map (\(API.BusinessHoursResp bh) -> {bhourId : bh.id, slot : fromMaybe "" bh.slot }) businessHours ),
        timeIntervals : if (isClosed headBH.specialDayType) then [] else filter (\timeIntervalInfo -> timeIntervalInfo.startTime /= "" || timeIntervalInfo.endTime /= "") (map (\(API.BusinessHoursResp bh) -> {bhourId : bh.id, startTime : fromMaybe "" bh.startTime, endTime : fromMaybe "" bh.endTime }) businessHours)
      }
    where
      isClosed specialDayType = maybe false (\val -> val == "Closed") specialDayType

  convertServiceBusinessHoursData :: API.BusinessHoursResp -> BusinessHoursData
  convertServiceBusinessHoursData (API.BusinessHoursResp resp) = do
    { bhourId : resp.id,
      slot : resp.slot,
      startTime : resp.startTime,
      endTime : resp.endTime ,
      categories : map (convertServiceTicketCategoriesData (length resp.categories)) resp.categories,
      operationalDays : resp.operationalDays
    }
  convertPeopleCategoriesData :: API.PeopleCategoriesResp -> PeopleCategoriesRespData
  convertPeopleCategoriesData (API.PeopleCategoriesResp price) = do
    { peopleCategoryName : price.name,
      pricePerUnit : ceil price.pricePerUnit,
      currentValue : 0,
      peopleCategoryId : price.id,
      ticketLimitCrossed : false
    }
  convertServiceTicketCategoriesData :: Int ->API.TicketCategoriesResp -> TicketCategoriesData
  convertServiceTicketCategoriesData catLength (API.TicketCategoriesResp resp) = do
    { categoryName : resp.name,
      categoryId : resp.id,
      availableSeats : resp.availableSeats,
      allowedSeats : resp.allowedSeats,
      bookedSeats : resp.bookedSeats,
      peopleCategories : map convertPeopleCategoriesData resp.peopleCategories,
      isSelected : catLength <= 1
    }

getValidTimeIntervals :: Array SlotsAndTimeIntervalData -> String -> Array TimeInterval
getValidTimeIntervals timeInData selOpDay = do
  let validSlotTIdata = find (\ti -> selOpDay `elem` ti.operationalDays) timeInData
  maybe [] (\validSlot -> validSlot.timeIntervals) validSlotTIdata

getValidBHid :: Array TimeInterval -> String -> String -> TicketBookingScreenState -> API.ServiceExpiry -> Maybe String
getValidBHid timeIntervals selOperationalDay dateOfVisit state expiry = do
    let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
        selTimeInterval = timeIntervals !! 0
    case selTimeInterval of
      Nothing -> Nothing
      Just sti -> do
        let startTime = if not (DS.null sti.startTime) then convertUTCTimeToISTTimeinHHMMSS sti.startTime else ""
            endTime = if not (DS.null  sti.endTime) then convertUTCTimeToISTTimeinHHMMSS sti.endTime else ""
        if dateOfVisit == currentDate then do
            case expiry of
              API.InstantExpiry val -> do
                if not (DS.null startTime) then do
                  let newStartTime = runFn3 incrOrDecrTimeFrom startTime val false
                  if (now > newStartTime) then 
                      if not (DS.null endTime) then do
                        if (now < endTime) then Just sti.bhourId
                        else Nothing 
                      else Just sti.bhourId
                  else Nothing
                else 
                    if not (DS.null endTime) then do
                      if (now < endTime) then Just sti.bhourId
                      else Nothing 
                    else Nothing
              _ ->  if not (DS.null endTime) then do
                      if (now < endTime) then Just sti.bhourId
                      else Nothing 
                    else Nothing
        else Just sti.bhourId

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