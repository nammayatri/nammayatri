{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.TicketList.Transformer where

import Prelude

import Accessor (_ticketShortId, _ticketPlaceId, _ticketPlaceName, _personId, _amount, _visitDate, _status, _services, _id, _categoryId, _name, _availableSeats, _allowedSeats, _bookedSeats, _peopleCategories, _isClosed)
import Data.Array (head, concat, sortBy, elem, find, groupBy, length, mapWithIndex, (!!), filter)
import Data.String (toUpper)
import Data.String as DS
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Services.API as API
import Data.Lens((^.))
import Screens.Types(PeopleCategoriesData, OperationalDaysData, FlattenedBusinessHourData, TicketServiceData, ServiceCategory, TimeInterval, TicketBookingScreenState(..), TicketBookingItem(..),IndividualBookingItem(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..), TicketBookingPeopleCategoryDetails(..), OperationalDate(..))
import Data.Array.NonEmpty as DAN
import Data.Int (ceil)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC)
import Data.Function.Uncurried (runFn3)
import Helpers.Utils(incrOrDecrTimeFrom)
import Data.Ord (compare)

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
    "CANCELLED" -> API.Cancelled
    _ -> API.Pending

transformRespToStateDatav2 :: Boolean -> API.TicketServiceResp -> TicketBookingScreenState -> String -> TicketServiceData
transformRespToStateDatav2 isFirstElement (API.TicketServiceResp service) state selOpDay =
  let serviceCatData = transformBusinessHoursToServiceCategoriesData service.businessHours selOpDay
      modifiedSelectedServiceCategoriesData = selectByDefaultOneServiceCategory serviceCatData selOpDay
  in
  { id : service.id,
    serviceName : service.name,
    allowFutureBooking : service.allowFutureBooking,
    shortDesc : service.shortDesc,
    expiry : service.expiry,
    isExpanded : isFirstElement,
    serviceCategories : modifiedSelectedServiceCategoriesData,
    selectedBHId : Nothing
  }
  where
    transformBusinessHoursToServiceCategoriesData :: Array API.BusinessHoursResp -> String -> Array ServiceCategory
    transformBusinessHoursToServiceCategoriesData respArr selOpDay =
      let flattenedBusinessHourData = flattenBusinessHourData respArr
          sortedServicesData = sortBy (\bh1 bh2-> compare (bh1.category ^. _id) (bh2.category ^. _id)) flattenedBusinessHourData
          groupedBusinessHoursAccordingToServiceCategories = groupBy (\bh1 bh2 -> (bh1.category ^. _id) == (bh2.category ^. _id)) sortedServicesData
      in concat $ map (generateServiceCategoryData selOpDay) groupedBusinessHoursAccordingToServiceCategories

    generateServiceCategoryData :: String -> DAN.NonEmptyArray FlattenedBusinessHourData -> Array ServiceCategory
    generateServiceCategoryData selOpDay respNEArr = do
      let respArr = DAN.toArray respNEArr
          operationalDaysData = generateOperationalDaysData respArr
      case head respArr of
        Nothing -> []
        Just bhData ->
          [{  categoryId : bhData.category ^. _id,
              categoryName : bhData.category ^. _name,
              availableSeats : bhData.category ^. _availableSeats,
              allowedSeats : bhData.category ^. _allowedSeats,
              bookedSeats :  bhData.category ^. _bookedSeats,
              isClosed : bhData.category ^. _isClosed,
              isSelected : false,
              peopleCategories : generatePeopleCategories (bhData.category ^. _peopleCategories),
              operationalDays : operationalDaysData,
              operationalDate : bhData.operationalDate,
              validOpDay : find (\opDay -> selOpDay `elem` opDay.operationalDays) operationalDaysData,
              noRemainingTicketAvailable : false
          }]

    generateOperationalDaysData :: Array FlattenedBusinessHourData -> Array OperationalDaysData
    generateOperationalDaysData respArr =
      let sortedData = sortBy (\bh1 bh2 -> compare bh1.operationalDays bh2.operationalDays) respArr
          groupedData = groupBy (\bh1 bh2 -> bh1.operationalDays == bh2.operationalDays) sortedData
      in map generateSlotData groupedData

    generateOperationalDateData :: Maybe API.OperationalDateResp -> Maybe OperationalDate
    generateOperationalDateData res = 
      case res of
        Just (API.OperationalDateResp resp) ->
          Just { startDate : resp.startDate,
                 endDate : resp.eneDate
                }
        Nothing -> Nothing

    generateSlotData :: DAN.NonEmptyArray FlattenedBusinessHourData -> OperationalDaysData
    generateSlotData bhs = 
      let headBH = DAN.head bhs
          businessHours = DAN.toArray bhs
      in
        { operationalDays : headBH.operationalDays,
          slot : if (isClosed headBH.specialDayType) then [] else filter (\x -> not DS.null x.slot ) (map (\bh -> {bhourId : bh.id, slot : fromMaybe "" bh.slot }) businessHours ),
          timeIntervals : if (isClosed headBH.specialDayType) then [] else filter (\x -> not DS.null x.startTime || not DS.null x.endTime ) (map (\bh -> {bhourId : bh.id, startTime : fromMaybe "" bh.startTime, endTime : fromMaybe "" bh.endTime }) businessHours)
        }
      where
        isClosed specialDayType = maybe false (\val -> val == "Closed") specialDayType

    generatePeopleCategories :: Array API.PeopleCategoriesResp -> Array PeopleCategoriesData
    generatePeopleCategories arr = map (\(API.PeopleCategoriesResp resp) -> 
          { peopleCategoryName : resp.name,
            pricePerUnit : ceil resp.pricePerUnit,
            currentValue : 0,
            peopleCategoryId : resp.id,
            ticketLimitCrossed : false
          }) arr

    flattenBusinessHourData :: Array API.BusinessHoursResp -> Array FlattenedBusinessHourData
    flattenBusinessHourData respArr = 
      concat
      ( map (\(API.BusinessHoursResp bh) ->
           map (\(API.TicketCategoriesResp cat) -> 
            { id : bh.id,
              slot : bh.slot,
              startTime : bh.startTime,
              endTime : bh.endTime,
              specialDayDescription : bh.specialDayDescription,
              specialDayType : bh.specialDayType,
              operationalDate : generateOperationalDateData bh.operationalDate,
              operationalDays : sortBy (\d1 d2 -> compare d1 d2) bh.operationalDays,
              category : (API.TicketCategoriesResp cat)
            }) bh.categories
        ) respArr
      )

    getCategoriesId :: Array API.TicketCategoriesResp -> Array String
    getCategoriesId categoryRespArr = map (\(API.TicketCategoriesResp x) -> x.id) categoryRespArr

selectByDefaultOneServiceCategory :: Array ServiceCategory -> String -> Array ServiceCategory
selectByDefaultOneServiceCategory arr selOpDay = 
  case (find (\serviceCat -> length (filter (\opDay -> selOpDay `elem` opDay.operationalDays) serviceCat.operationalDays) > 0 ) arr) of
    Nothing -> map (\elem -> elem {isSelected = false}) arr
    Just serviceCat -> map (\elem -> if elem.categoryId == serviceCat.categoryId then elem {isSelected = true} else elem {isSelected = false}) arr