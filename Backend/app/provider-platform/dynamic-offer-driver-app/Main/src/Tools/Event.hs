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

import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as DBooking
import Domain.Types.DriverQuote
import qualified Domain.Types.Estimate as ES
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSearchRequest
import Domain.Types.SearchTry
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.JSON (constructorsWithSnakeCase)
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
        vehVar :: DVST.ServiceTierType, --vehicle variant
        cAt :: UTCTime
      }
  | Exophone
      { vendor :: Maybe Text,
        callType :: Maybe Text,
        callSid :: Maybe Text,
        status :: Maybe Text,
        exophoneNumber :: Maybe Text,
        rideId :: Maybe (Id DRide.Ride)
      }
  | SDK Text
  | EventTrackerPayLoad
      { createdAt :: UTCTime,
        entity :: Text,
        entityFieldName :: Text,
        entityPrimaryId :: Text,
        eventName :: EventName,
        fromState :: Maybe Text,
        id :: Text,
        reason :: Maybe Text,
        subscriptionServiceName :: Maybe Text,
        toState :: Maybe Text,
        merchantId :: Maybe (Id Merchant),
        merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
        updatedAt :: UTCTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON Payload where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON Payload where
  parseJSON = genericParseJSON constructorsWithSnakeCase

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

data ExophoneEventData = ExophoneEventData
  { vendor :: Maybe Text,
    callType :: Maybe Text,
    rideId :: Maybe (Id DRide.Ride),
    callSid :: Maybe Text,
    status :: Maybe Text,
    merchantId :: Maybe (Id Merchant),
    triggeredBy :: EventTriggeredBy,
    personId :: Maybe (Id Person),
    exophoneNumber :: Maybe Text
  }

data SDKEventData = SDKEventData
  { personId :: Maybe (Id Person),
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    payload :: Text
  }

data EventTrackerData = EventTrackerData
  { id :: Text,
    entity :: Text,
    entityFieldName :: Text,
    entityPrimaryId :: Text,
    eventName :: EventName,
    fromState :: Maybe Text,
    reason :: Maybe Text,
    subscriptionServiceName :: Maybe Text,
    toState :: Maybe Text,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

data EventName = DRIVER_FEE_AUTO_PAY_TO_MANUAL | AUTO_PAY_STATUS_TOGGLE | SERVICE_USAGE_CHARGE_TOGGLE | REFUND_SECURITY_DEPOSIT | DRIVER_DUES_AND_PLAN_LIMIT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

triggerEstimateEvent ::
  ( EventStreamFlow m r
  ) =>
  EstimateEventData ->
  m ()
triggerEstimateEvent estimateData = do
  let estimatePayload = Estimates {eId = estimateData.estimate.id, srId = estimateData.estimate.requestId, vehVar = estimateData.estimate.vehicleServiceTier, cAt = estimateData.estimate.createdAt}
  estEnvt <- createEvent Nothing (getId estimateData.merchantId) Estimate DYNAMIC_OFFER_DRIVER_APP System (Just estimatePayload) (Just $ getId estimateData.estimate.id) Nothing
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
  envt <- createEvent (Just $ getId quoteData.quote.driverId) (getId quoteData.quote.providerId) Quotes DYNAMIC_OFFER_DRIVER_APP System (Just quotePayload) (Just $ getId quoteData.quote.id) Nothing
  triggerEvent envt

triggerSearchEvent ::
  ( EventStreamFlow m r
  ) =>
  SearchEventData ->
  m ()
triggerSearchEvent searchData = do
  let searchPayload = Search {sId = searchData.searchRequest.id, cAt = searchData.searchRequest.createdAt}
  envt <- createEvent Nothing (getId searchData.merchantId) SearchRequest DYNAMIC_OFFER_DRIVER_APP System (Just searchPayload) (Just $ getId searchData.searchRequest.id) (Just $ getId searchData.searchRequest.merchantOperatingCityId)
  triggerEvent envt

triggerRideEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  RideEventData ->
  m ()
triggerRideEvent eventType rideData = do
  let ridePayload = Ride {rId = rideData.ride.id, rs = rideData.ride.status, cAt = rideData.ride.createdAt, uAt = rideData.ride.updatedAt}
  envt <- createEvent (Just $ getId rideData.personId) (getId rideData.merchantId) eventType DYNAMIC_OFFER_DRIVER_APP System (Just ridePayload) (Just $ getId rideData.ride.id) (Just $ getId rideData.ride.merchantOperatingCityId)
  triggerEvent envt

triggerBookingEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  BookingEventData ->
  m ()
triggerBookingEvent eventType bookingData = do
  let bookingPayload = Booking {bId = bookingData.booking.id, bs = bookingData.booking.status, cAt = bookingData.booking.createdAt, uAt = bookingData.booking.updatedAt}
  event <- createEvent (Just $ getId bookingData.personId) (getId bookingData.merchantId) eventType DYNAMIC_OFFER_DRIVER_APP System (Just bookingPayload) (Just $ getId bookingData.booking.id) (Just $ getId bookingData.booking.merchantOperatingCityId)
  triggerEvent event

triggerExophoneEvent ::
  ( EventStreamFlow m r
  ) =>
  ExophoneEventData ->
  m ()
triggerExophoneEvent ExophoneEventData {..} = do
  let exophonePayload = Exophone {..}
  exoevent <- createEvent (getId <$> personId) (maybe "" getId merchantId) ExophoneData DYNAMIC_OFFER_DRIVER_APP triggeredBy (Just exophonePayload) Nothing Nothing
  triggerEvent exoevent

triggerEventTrackerEvent ::
  ( EventStreamFlow m r
  ) =>
  EventTrackerData ->
  m ()
triggerEventTrackerEvent EventTrackerData {..} = do
  let eventTrackerPayload = EventTrackerPayLoad {..}
  event <- createEvent Nothing (maybe "" getId merchantId) EventTracker DYNAMIC_OFFER_DRIVER_APP System (Just eventTrackerPayload) (Just id) (getId <$> merchantOperatingCityId)
  triggerEvent event
