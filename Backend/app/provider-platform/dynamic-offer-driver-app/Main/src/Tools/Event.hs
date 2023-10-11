{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Tools.Event where

import qualified Domain.Types.Booking as DBooking
import Domain.Types.DriverQuote
import qualified Domain.Types.Estimate as ES
import Domain.Types.Merchant
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequestSpecialZone as SpSearchRequest
import Domain.Types.SearchTry
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Id
import Lib.SessionizerMetrics.EventStream
import Lib.SessionizerMetrics.Types.Event

data Payload
  = Ride
      { rId :: Id DRide.Ride, -- rideId
        rs :: DRide.RideStatus, -- status
        cAt :: UTCTime, -- createdTime
        uAt :: UTCTime
      }
  | Booking
      { bId :: Id DBooking.Booking, -- bookingId
        bs :: DBooking.BookingStatus, -- status
        cAt :: UTCTime, -- createdTime
        uAt :: UTCTime -- UpdatedAtTime
      }
  | Search
      { sId :: Either (Id DSearchRequest.SearchRequest) (Id SpSearchRequest.SearchRequestSpecialZone),
        cAt :: UTCTime
      }
  | Quote
      { qId :: Id DriverQuote,
        st :: DriverQuoteStatus,
        searchReqId :: Id DSearchRequest.SearchRequest,
        searchTryId :: Id SearchTry,
        cAt :: UTCTime
      }
  | Estimates
      { eId :: Id ES.Estimate,
        srId :: Id DSearchRequest.SearchRequest, --searchReqId
        vehVar :: Variant.Variant, --vehicle variant
        cAt :: UTCTime
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RideEventData = RideEventData
  { ride :: DRide.Ride,
    personId :: Id Person,
    merchantId :: Id Merchant
  }

data BookingEventData = BookingEventData
  { booking :: DBooking.Booking,
    personId :: Id Person,
    merchantId :: Id Merchant
  }

data EstimateEventData = EstimateEventData
  { estimate :: ES.Estimate,
    merchantId :: Id Merchant
  }

newtype QuoteEventData = QuoteEventData
  { quote :: DriverQuote
  }

data SearchEventData = SearchEventData
  { searchRequest :: Either DSearchRequest.SearchRequest SpSearchRequest.SearchRequestSpecialZone,
    merchantId :: Id Merchant
  }

triggerEstimateEvent ::
  ( EventStreamFlow m r
  ) =>
  EstimateEventData ->
  m ()
triggerEstimateEvent estimateData = do
  let estimatePayload = Estimates {eId = estimateData.estimate.id, srId = estimateData.estimate.requestId, vehVar = estimateData.estimate.vehicleVariant, cAt = estimateData.estimate.createdAt}
  estEnvt <- createEvent Nothing (getId estimateData.merchantId) Estimate DYNAMIC_OFFER_DRIVER_APP System (Just estimatePayload) (Just $ getId estimateData.estimate.id)
  triggerEvent estEnvt

triggerRideEndEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideEndEvent = triggerRideEvent RideEnded

triggerRideStartEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideStartEvent = triggerRideEvent RideStarted

triggerRideCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCancelledEvent = triggerRideEvent RideCancelled

triggerRideCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCreatedEvent = triggerRideEvent RideCreated

triggerBookingCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCreatedEvent = triggerBookingEvent BookingCreated

triggerBookingCompletedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCompletedEvent = triggerBookingEvent BookingCompleted

triggerBookingCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCancelledEvent = triggerBookingEvent BookingCancelled

triggerQuoteEvent ::
  ( EventStreamFlow m r
  ) =>
  QuoteEventData ->
  m ()
triggerQuoteEvent quoteData = do
  let quotePayload = Quote {qId = quoteData.quote.id, st = quoteData.quote.status, searchReqId = quoteData.quote.requestId, searchTryId = quoteData.quote.searchTryId, cAt = quoteData.quote.createdAt}
  envt <- createEvent (Just $ getId quoteData.quote.driverId) (getId quoteData.quote.providerId) Quotes DYNAMIC_OFFER_DRIVER_APP System (Just quotePayload) (Just $ getId quoteData.quote.id)
  triggerEvent envt

triggerSearchEvent ::
  ( EventStreamFlow m r
  ) =>
  SearchEventData ->
  m ()
triggerSearchEvent searchData = do
  envt <- case searchData.searchRequest of
    Left searchReq -> do
      let searchPayload = Search {sId = Left searchReq.id, cAt = searchReq.createdAt}
      createEvent Nothing (getId searchData.merchantId) SearchRequest DYNAMIC_OFFER_DRIVER_APP System (Just searchPayload) (Just $ getId searchReq.id)
    Right searchReqSpecialZone -> do
      let searchPayload = Search {sId = Right searchReqSpecialZone.id, cAt = searchReqSpecialZone.createdAt}
      createEvent Nothing (getId searchData.merchantId) SearchRequest DYNAMIC_OFFER_DRIVER_APP System (Just searchPayload) (Just $ getId searchReqSpecialZone.id)
  triggerEvent envt

triggerRideEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  RideEventData ->
  m ()
triggerRideEvent eventType rideData = do
  let ridePayload = Ride {rId = rideData.ride.id, rs = rideData.ride.status, cAt = rideData.ride.createdAt, uAt = rideData.ride.updatedAt}
  envt <- createEvent (Just $ getId rideData.personId) (getId rideData.merchantId) eventType DYNAMIC_OFFER_DRIVER_APP System (Just ridePayload) (Just $ getId rideData.ride.id)
  triggerEvent envt

triggerBookingEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  BookingEventData ->
  m ()
triggerBookingEvent eventType bookingData = do
  let bookingPayload = Booking {bId = bookingData.booking.id, bs = bookingData.booking.status, cAt = bookingData.booking.createdAt, uAt = bookingData.booking.updatedAt}
  event <- createEvent (Just $ getId bookingData.personId) (getId bookingData.merchantId) eventType DYNAMIC_OFFER_DRIVER_APP System (Just bookingPayload) (Just $ getId bookingData.booking.id)
  triggerEvent event
