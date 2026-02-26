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
import qualified Domain.Types.BookingStatus as DBooking
import qualified Domain.Types.Estimate as ES
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.SearchRequest as DSearchRequest
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Domain.Types.VehicleVariant as DV
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
      { qId :: Id DQuote.Quote,
        searchReqId :: Id DSearchRequest.SearchRequest,
        cAt :: UTCTime
      }
  | Estimates
      { eId :: Id ES.Estimate,
        srId :: Id DSearchRequest.SearchRequest, --searchReqId
        vehVar :: VehicleVariant, --vehicle variant
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
  | AutoComplete
      { autocompleteInputs :: Text,
        customerId :: Id Person,
        id :: Text,
        isLocationSelectedOnMap :: Maybe Bool,
        searchRequestId :: Maybe (Id DSearchRequest.SearchRequest),
        searchType :: Text,
        sessionToken :: Text,
        merchantId :: Id Merchant,
        merchantOperatingCityId :: Id MerchantOperatingCity,
        originLat :: Text,
        originLon :: Text,
        createdAt :: UTCTime,
        updatedAt :: UTCTime
      }
  | RouteCollectionData
      { mapsProvider :: Maybe Text,
        routes :: [Text],
        searchRequestId :: Maybe (Id DSearchRequest.SearchRequest),
        merchantId :: Id Merchant,
        merchantOperatingCityId :: Id MerchantOperatingCity,
        createdAt :: UTCTime,
        updatedAt :: UTCTime
      }
  | MarketingParams
      { personId :: Id Person,
        gclId :: Maybe Text,
        utmCampaign :: Maybe Text,
        utmContent :: Maybe Text,
        utmCreativeFormat :: Maybe Text,
        utmMedium :: Maybe Text,
        utmSource :: Maybe Text,
        utmTerm :: Maybe Text,
        appName :: Maybe Text,
        deviceId :: Maybe Text,
        userType :: Maybe UserType,
        merchantId :: Id Merchant,
        merchantOperatingCityId :: Id MerchantOperatingCity,
        createdAt :: UTCTime,
        updatedAt :: UTCTime
      }
  | MarketingParamsPreLogin
      { gclId :: Maybe Text,
        utmCampaign :: Maybe Text,
        utmContent :: Maybe Text,
        utmCreativeFormat :: Maybe Text,
        utmMedium :: Maybe Text,
        utmSource :: Maybe Text,
        utmTerm :: Maybe Text,
        appName :: Maybe Text,
        deviceId :: Maybe Text,
        userType :: Maybe UserType,
        createdAt :: UTCTime,
        updatedAt :: UTCTime
      }
  deriving (Show, Eq, Generic, ToSchema)

instance ToJSON Payload where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON Payload where
  parseJSON = genericParseJSON constructorsWithSnakeCase

data RideEventData = RideEventData
  { ride :: DRide.Ride,
    personId :: Id Person,
    merchantId :: Id Merchant
  }

newtype BookingEventData = BookingEventData
  { booking :: DBooking.Booking
  }

data EstimateEventData = EstimateEventData
  { estimate :: ES.Estimate,
    personId :: Id Person,
    merchantId :: Id Merchant
  }

data QuoteEventData = QuoteEventData
  { quote :: DQuote.Quote,
    person :: Person,
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

data AutoCompleteEventData = AutoCompleteEventData
  { autocompleteInputs :: Text,
    customerId :: Id Person,
    id :: Text,
    isLocationSelectedOnMap :: Maybe Bool,
    searchRequestId :: Maybe (Id DSearchRequest.SearchRequest),
    searchType :: Text,
    sessionToken :: Text,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    originLat :: Text,
    originLon :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RouteDataEvent = RouteDataEvent
  { mapsProvider :: Maybe Text,
    routes :: [Text],
    searchRequestId :: Maybe (Id DSearchRequest.SearchRequest),
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON)

data MarketingParamsEventPreLoginData = MarketingParamsEventPreLoginData
  { gclId :: Maybe Text,
    utmCampaign :: Maybe Text,
    utmContent :: Maybe Text,
    utmCreativeFormat :: Maybe Text,
    utmMedium :: Maybe Text,
    utmSource :: Maybe Text,
    utmTerm :: Maybe Text,
    appName :: Maybe Text,
    deviceId :: Maybe Text,
    userType :: Maybe UserType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MarketingParamsEventData = MarketingParamsEventData
  { personId :: Id Person,
    gclId :: Maybe Text,
    utmCampaign :: Maybe Text,
    utmContent :: Maybe Text,
    utmCreativeFormat :: Maybe Text,
    utmMedium :: Maybe Text,
    utmSource :: Maybe Text,
    utmTerm :: Maybe Text,
    appName :: Maybe Text,
    deviceId :: Maybe Text,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    userType :: Maybe UserType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UserType = OLD | NEW deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype SearchEventData = SearchEventData
  { searchRequest :: DSearchRequest.SearchRequest
  }

triggerEstimateEvent ::
  ( EventStreamFlow m r
  ) =>
  EstimateEventData ->
  m ()
triggerEstimateEvent estimateData = do
  let estimatePayload = Estimates {eId = estimateData.estimate.id, srId = estimateData.estimate.requestId, vehVar = DV.castServiceTierToVariant estimateData.estimate.vehicleServiceTierType, cAt = estimateData.estimate.createdAt}
  estEnvt <- createEvent (Just $ getId estimateData.personId) (getId estimateData.merchantId) Estimate RIDER_APP System (Just estimatePayload) (Just $ getId estimateData.estimate.id) (getId <$> estimateData.estimate.merchantOperatingCityId)
  triggerEvent estEnvt

triggerRideCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCreatedEvent = triggerRideEvent RideCreated

triggerRideEndEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideEndEvent = triggerRideEvent RideEnded

triggerRideCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideCancelledEvent = triggerRideEvent RideCancelled

triggerRideStartedEvent ::
  ( EventStreamFlow m r
  ) =>
  RideEventData ->
  m ()
triggerRideStartedEvent = triggerRideEvent RideStarted

triggerBookingCreatedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCreatedEvent = triggerBookingEvent BookingCreated

triggerBookingCancelledEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCancelledEvent = triggerBookingEvent BookingCancelled

triggerBookingCompletedEvent ::
  ( EventStreamFlow m r
  ) =>
  BookingEventData ->
  m ()
triggerBookingCompletedEvent = triggerBookingEvent BookingCompleted

triggerQuoteEvent ::
  ( EventStreamFlow m r
  ) =>
  QuoteEventData ->
  m ()
triggerQuoteEvent quoteData = do
  let quotePayload = Quote {qId = quoteData.quote.id, searchReqId = quoteData.quote.requestId, cAt = quoteData.quote.createdAt}
  envt <- createEvent (Just $ getId quoteData.person.id) (getId quoteData.merchantId) Quotes RIDER_APP System (Just quotePayload) (Just $ getId quoteData.quote.id) (Just $ getId quoteData.person.merchantOperatingCityId)
  triggerEvent envt

triggerSearchEvent ::
  ( EventStreamFlow m r
  ) =>
  SearchEventData ->
  m ()
triggerSearchEvent searchData = do
  let searchPayload = Search {sId = searchData.searchRequest.id, cAt = searchData.searchRequest.createdAt}
  envt <- createEvent (Just $ getId searchData.searchRequest.riderId) (getId searchData.searchRequest.merchantId) SearchRequest RIDER_APP System (Just searchPayload) (Just $ getId searchData.searchRequest.id) (Just $ getId searchData.searchRequest.merchantOperatingCityId)
  triggerEvent envt

triggerRideEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  RideEventData ->
  m ()
triggerRideEvent eventType rideData = do
  let ridePayload = Ride {rId = rideData.ride.id, rs = rideData.ride.status, cAt = rideData.ride.createdAt, uAt = rideData.ride.updatedAt}
  envt <- createEvent (Just $ getId rideData.personId) (getId rideData.merchantId) eventType RIDER_APP System (Just ridePayload) (Just $ getId rideData.ride.id) $ getId <$> rideData.ride.merchantOperatingCityId
  triggerEvent envt

triggerBookingEvent ::
  ( EventStreamFlow m r
  ) =>
  EventType ->
  BookingEventData ->
  m ()
triggerBookingEvent eventType bookingData = do
  let bookingPayload = Booking {bId = bookingData.booking.id, bs = bookingData.booking.status, cAt = bookingData.booking.createdAt, uAt = bookingData.booking.updatedAt}
  event <- createEvent (Just $ getId bookingData.booking.riderId) (getId bookingData.booking.merchantId) eventType RIDER_APP System (Just bookingPayload) (Just $ getId bookingData.booking.id) (Just $ getId bookingData.booking.merchantOperatingCityId)
  triggerEvent event

triggerExophoneEvent ::
  ( EventStreamFlow m r
  ) =>
  ExophoneEventData ->
  m ()
triggerExophoneEvent ExophoneEventData {..} = do
  let exophonePayload = Exophone {..}
  exoevent <- createEvent (getId <$> personId) (maybe "" getId merchantId) ExophoneData RIDER_APP triggeredBy (Just exophonePayload) Nothing Nothing
  triggerEvent exoevent

triggerAutoCompleteEvent ::
  ( EventStreamFlow m r
  ) =>
  AutoCompleteEventData ->
  m ()
triggerAutoCompleteEvent AutoCompleteEventData {..} = do
  let autoCompletePayload = AutoComplete {..}
  event <- createEvent (Just $ getId autoCompletePayload.customerId) (getId autoCompletePayload.merchantId) AutoCompleteData RIDER_APP System (Just autoCompletePayload) Nothing Nothing
  triggerEvent event

triggerRouteDataEvent ::
  ( EventStreamFlow m r
  ) =>
  RouteDataEvent ->
  m ()
triggerRouteDataEvent RouteDataEvent {..} = do
  let routePayload = RouteCollectionData {..}
  event <- createEvent Nothing (getId routePayload.merchantId) RouteCollection RIDER_APP System (Just routePayload) Nothing Nothing
  triggerEvent event

triggerMarketingParamEvent ::
  ( EventStreamFlow m r
  ) =>
  MarketingParamsEventData ->
  m ()
triggerMarketingParamEvent MarketingParamsEventData {..} = do
  let marketingParamsPayload = MarketingParams {..}
  event <- createEvent (Just $ getId marketingParamsPayload.personId) (getId marketingParamsPayload.merchantId) MarketingParamsData RIDER_APP System (Just marketingParamsPayload) Nothing Nothing
  triggerEvent event

triggerMarketingParamEventPreLogin ::
  ( EventStreamFlow m r
  ) =>
  MarketingParamsEventPreLoginData ->
  m ()
triggerMarketingParamEventPreLogin MarketingParamsEventPreLoginData {..} = do
  let marketingParamsPayload = MarketingParamsEventPreLoginData {..}
  event <- createEvent (Just $ "") (fromMaybe "" appName) MarketingParamsPreLoginData RIDER_APP System (Just marketingParamsPayload) Nothing Nothing
  triggerEvent event
