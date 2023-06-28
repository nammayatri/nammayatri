{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Event where

import qualified Domain.Types.Booking as DBooking
import Domain.Types.DriverQuote
import qualified Domain.Types.Estimate as ES
import Domain.Types.Merchant
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSearchRequest
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
      { sId :: Id DSearchRequest.SearchRequest,
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
  { searchRequest :: DSearchRequest.SearchRequest,
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
triggerRideEndEvent rideData = do
  triggerRideEvent rideData RideEnded

triggerRideStartEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideStartEvent rideData = do
  triggerRideEvent rideData RideStarted

triggerRideCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCancelledEvent rideData = do
  triggerRideEvent rideData RideCancelled

triggerRideCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCreatedEvent rideData = do
  triggerRideEvent rideData RideCreated

triggerBookingCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCreatedEvent bookingData = do
  triggerBookingEvent bookingData BookingCreated

triggerBookingCompletedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCompletedEvent bookingData = do
  triggerBookingEvent bookingData BookingCompleted

triggerBookingCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCancelledEvent bookingData = do
  triggerBookingEvent bookingData BookingCancelled

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
  let searchPayload = Search {sId = searchData.searchRequest.id, cAt = searchData.searchRequest.createdAt}
  envt <- createEvent Nothing (getId searchData.merchantId) SearchRequest DYNAMIC_OFFER_DRIVER_APP System (Just searchPayload) (Just $ getId searchData.searchRequest.id)
  triggerEvent envt

triggerRideEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  EventType ->
  m ()
triggerRideEvent rideData eventType = do
  let ridePayload = Ride {rId = rideData.ride.id, rs = rideData.ride.status, cAt = rideData.ride.createdAt, uAt = rideData.ride.updatedAt}
  envt <- createEvent (Just $ getId rideData.personId) (getId rideData.merchantId) eventType RIDER_APP System (Just ridePayload) (Just $ getId rideData.ride.id)
  triggerEvent envt

triggerBookingEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  EventType ->
  m ()
triggerBookingEvent bookingData eventType = do
  let bookingPayload = Booking {bId = bookingData.booking.id, bs = bookingData.booking.status, cAt = bookingData.booking.createdAt, uAt = bookingData.booking.updatedAt}
  event <- createEvent (Just $ getId bookingData.personId) (getId bookingData.merchantId) eventType RIDER_APP System (Just bookingPayload) (Just $ getId bookingData.booking.id)
  triggerEvent event
