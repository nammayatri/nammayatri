{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.PublicTransportQuote
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.Ticket as DTT
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.Ticket as QRideT
import Tools.Error
import Tools.Event

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quoteId :: Id DQuote.Quote,
    quantity :: Maybe Integer,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod)
  }

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    booking :: DRB.Booking,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    riderEmail :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }
  deriving (Show, Generic)

data DConfirmBusRes = DConfirmBusRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    ticket :: DTT.Ticket,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    riderEmail :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    city :: Context.City,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails Text (Maybe Text)
  | ConfirmOneWaySpecialZoneDetails Text
  | ConfirmPublicTransportDetails PublicTransportQuoteAPIEntity
  deriving (Show, Generic)

confirmBus ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  DConfirmReq ->
  m DConfirmBusRes
confirmBus DConfirmReq {..} = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  fulfillmentId <-
    case quote.quoteDetails of
      DQuote.OneWayDetails _ -> pure Nothing
      DQuote.RentalDetails _ -> pure Nothing
      DQuote.DriverOfferDetails driverOffer -> do
        estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
        when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
        when (driverOffer.validTill < now) $
          throwError $ QuoteExpired quote.id.getId
        pure (Just estimate.bppEstimateId.getId)
      DQuote.OneWaySpecialZoneDetails details -> pure (Just details.quoteId)
      DQuote.PublicTransportQuoteDetails details -> pure (Just details.quoteId)
  ticket <- buildTicket searchRequest fulfillmentId quote fromLocation mbToLocation now quantity
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  details <- mkConfirmQuoteDetails quote.quoteDetails Nothing
  riderPhone <- mapM decrypt person.mobileNumber
  riderEmail <- mapM decrypt person.email
  let riderName = person.firstName
  moc <- CQMOC.findById ticket.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist ticket.merchantOperatingCityId.getId)
  merchant <- CQM.findById moc.merchantId >>= fromMaybeM (MerchantNotFound moc.merchantId.getId)
  paymentMethod <- forM paymentMethodId $ \paymentMethodId' -> do
    paymentMethod <-
      CQMPM.findByIdAndMerchantOperatingCityId paymentMethodId' ticket.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId'.getId)
    unless (paymentMethodId' `elem` searchRequest.availablePaymentMethods) $
      throwError (InvalidRequest "Payment method not allowed")
    pure paymentMethod

  -- DB.runTransaction $ do
  _ <- QRideT.create ticket
  return $
    DConfirmBusRes
      { ticket = ticket,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        itemId = quote.itemId,
        fromLoc = fromLocation,
        toLoc = mbToLocation,
        quoteDetails = details,
        vehicleVariant = quote.vehicleVariant,
        searchRequestId = searchRequest.id,
        paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> paymentMethod,
        maxEstimatedDistance = Nothing,
        city = moc.city,
        ..
      }
  where
    mkConfirmQuoteDetails quoteDetails fulfillmentId = do
      case quoteDetails of
        DQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
        DQuote.RentalDetails RentalSlab {..} -> pure $ ConfirmRentalDetails $ RentalSlabAPIEntity {..}
        DQuote.DriverOfferDetails driverOffer -> do
          bppEstimateId <- fulfillmentId & fromMaybeM (InternalError "FulfillmentId not found in Init. this error should never come.")
          pure $ ConfirmAutoDetails bppEstimateId driverOffer.driverId
        DQuote.OneWaySpecialZoneDetails details -> pure $ ConfirmOneWaySpecialZoneDetails details.quoteId
        DQuote.PublicTransportQuoteDetails details -> pure $ ConfirmPublicTransportDetails $ PublicTransportQuoteAPIEntity {quoteId = details.quoteId} -- Only this case should happen

confirm ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  fulfillmentId <-
    case quote.quoteDetails of
      DQuote.OneWayDetails _ -> pure Nothing
      DQuote.RentalDetails _ -> pure Nothing
      DQuote.DriverOfferDetails driverOffer -> do
        estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
        when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
        when (driverOffer.validTill < now) $
          throwError $ QuoteExpired quote.id.getId
        pure (Just estimate.bppEstimateId.getId)
      DQuote.OneWaySpecialZoneDetails details -> pure (Just details.quoteId)
      DQuote.PublicTransportQuoteDetails details -> pure (Just details.quoteId)
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatus personId DRB.activeBookingStatus
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
      driverId = getDriverId quote.quoteDetails
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  exophone <- findRandomExophone merchantOperatingCityId
  booking <- buildBooking searchRequest fulfillmentId quote fromLocation mbToLocation exophone now Nothing paymentMethodId driverId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderPhone <- mapM decrypt person.mobileNumber
  riderEmail <- mapM decrypt person.email
  let riderName = person.firstName
  triggerBookingCreatedEvent BookingEventData {booking = booking}
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  details <- mkConfirmQuoteDetails quote.quoteDetails fulfillmentId
  paymentMethod <- forM paymentMethodId $ \paymentMethodId' -> do
    paymentMethod <-
      CQMPM.findByIdAndMerchantOperatingCityId paymentMethodId' merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId'.getId)
    unless (paymentMethodId' `elem` searchRequest.availablePaymentMethods) $
      throwError (InvalidRequest "Payment method not allowed")
    pure paymentMethod

  -- DB.runTransaction $ do
  _ <- QRideB.createBooking booking
  _ <- QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
  _ <- QEstimate.updateStatusByRequestId quote.requestId DEstimate.COMPLETED
  QPFS.clearCache searchRequest.riderId
  return $
    DConfirmRes
      { booking = booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        itemId = quote.itemId,
        fromLoc = fromLocation,
        toLoc = mbToLocation,
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        searchRequestId = searchRequest.id,
        maxEstimatedDistance = searchRequest.maxDistance,
        paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> paymentMethod,
        ..
      }
  where
    mkConfirmQuoteDetails quoteDetails fulfillmentId = do
      case quoteDetails of
        DQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
        DQuote.RentalDetails RentalSlab {..} -> pure $ ConfirmRentalDetails $ RentalSlabAPIEntity {..}
        DQuote.DriverOfferDetails driverOffer -> do
          bppEstimateId <- fulfillmentId & fromMaybeM (InternalError "FulfillmentId not found in Init. this error should never come.")
          pure $ ConfirmAutoDetails bppEstimateId driverOffer.driverId
        DQuote.OneWaySpecialZoneDetails details -> pure $ ConfirmOneWaySpecialZoneDetails details.quoteId
        DQuote.PublicTransportQuoteDetails details -> pure $ ConfirmPublicTransportDetails $ PublicTransportQuoteAPIEntity {quoteId = details.quoteId}
    getDriverId :: DQuote.QuoteDetails -> Maybe Text
    getDriverId = \case
      DQuote.DriverOfferDetails driverOffer -> driverOffer.driverId
      _ -> Nothing

buildTicket ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  Maybe Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  UTCTime ->
  Maybe Integer ->
  m DTT.Ticket
buildTicket searchRequest mbFulfillmentId quote fromLoc _ now mbQuantity = do
  id <- generateGUID
  let quantity = fromMaybe 1 mbQuantity
  let pricePerAdult = quote.estimatedFare
  let totalPrice = pricePerAdult * fromIntegral quantity
  return $
    DTT.Ticket
      { id = Id id,
        status = DTT.INIT,
        quoteId = Just quote.id,
        bppTicketId = Nothing,
        searchRequestId = searchRequest.id.getId,
        fulfillmentId = mbFulfillmentId,
        providerId = quote.providerId,
        bppOrderId = Nothing,
        paymentUrl = Nothing,
        itemId = quote.itemId,
        providerUrl = quote.providerUrl,
        qrData = Nothing,
        createdAt = now,
        updatedAt = now,
        merchantOperatingCityId = searchRequest.merchantOperatingCityId,
        fromLocation = fromLoc,
        ..
      }

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  Maybe Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  Maybe Text ->
  m DRB.Booking
buildBooking searchRequest mbFulfillmentId quote fromLoc mbToLoc exophone now otpCode paymentMethodId driverId = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        driverId,
        fulfillmentId = mbFulfillmentId,
        quoteId = Just quote.id,
        paymentMethodId,
        paymentUrl = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        primaryExophone = exophone.primaryPhone,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
        itemId = quote.itemId,
        providerMobileNumber = quote.providerMobileNumber,
        startTime = searchRequest.startTime,
        riderId = searchRequest.riderId,
        fromLocation = fromLoc,
        estimatedFare = quote.estimatedFare,
        discount = quote.discount,
        estimatedTotalFare = quote.estimatedTotalFare,
        vehicleVariant = quote.vehicleVariant,
        bookingDetails,
        tripTerms = quote.tripTerms,
        merchantId = searchRequest.merchantId,
        merchantOperatingCityId = searchRequest.merchantOperatingCityId,
        specialLocationTag = quote.specialLocationTag,
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalSlab -> pure $ DRB.RentalDetails rentalSlab
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
      DQuote.PublicTransportQuoteDetails _ -> throwError (InvalidRequest "Unable to build Booking details from Public Transport Quote Details. This error should never occur")
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DExophone.Exophone
findRandomExophone merchantOperatingCityId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  exophones <- CQExophone.findByMerchantOperatingCityIdAndService merchantOperatingCityId merchantServiceUsageConfig.getExophone
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantOperatingCityId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones
