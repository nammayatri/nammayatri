{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Confirm
  ( confirm,
    cancelBooking,
    DConfirmRes (..),
    ConfirmQuoteDetails (..),
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Estimate (checkIfEstimateCancelled)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

-- domain types

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    startTime :: UTCTime,
    booking :: DRB.Booking,
    searchRequestId :: Id DSReq.SearchRequest
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails (Id DDriverOffer.BPPQuote)
  deriving (Show, Generic)

confirm :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Id DQuote.Quote -> m DConfirmRes
confirm personId quoteId = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure ()
    DQuote.RentalDetails _ -> pure ()
    DQuote.DriverOfferDetails driverOffer -> do
      estimate <- DB.runInReplica $ QEstimate.findOneEstimateByRequestId quote.requestId >>= fromMaybeM EstimateNotFound
      checkIfEstimateCancelled estimate.id estimate.status
      when (driverOffer.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatus personId DRB.activeBookingStatus
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  bFromLocation <- buildBookingLocation now fromLocation
  mbBToLocation <- traverse (buildBookingLocation now) mbToLocation
  booking <- buildBooking searchRequest quote bFromLocation mbBToLocation now
  let details = mkConfirmQuoteDetails quote.quoteDetails
  DB.runTransaction $ do
    QRideB.create booking
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
    QEstimate.updateStatusbyRequestId quote.requestId $ Just DEstimate.COMPLETED
  return $
    DConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        fromLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon},
        toLoc = mbToLocation <&> \toLocation -> LatLong {lat = toLocation.lat, lon = toLocation.lon},
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        startTime = searchRequest.startTime,
        searchRequestId = searchRequest.id
      }
  where
    mkConfirmQuoteDetails :: DQuote.QuoteDetails -> ConfirmQuoteDetails
    mkConfirmQuoteDetails = \case
      DQuote.OneWayDetails _ -> ConfirmOneWayDetails
      DQuote.RentalDetails RentalSlab {..} -> ConfirmRentalDetails $ RentalSlabAPIEntity {..}
      DQuote.DriverOfferDetails driverOffer -> ConfirmAutoDetails driverOffer.bppQuoteId

buildBookingLocation :: MonadGuid m => UTCTime -> DSRLoc.SearchReqLocation -> m DBL.BookingLocation
buildBookingLocation now DSRLoc.SearchReqLocation {..} = do
  locId <- generateGUID
  return
    DBL.BookingLocation
      { id = locId,
        lat,
        lon,
        address,
        createdAt = now,
        updatedAt = now
      }

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  DQuote.Quote ->
  DBL.BookingLocation ->
  Maybe DBL.BookingLocation ->
  UTCTime ->
  m DRB.Booking
buildBooking searchRequest quote fromLoc mbToLoc now = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        quoteId = Just $ quote.id,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
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
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalSlab -> pure $ DRB.RentalDetails rentalSlab
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}

-- cancel booking when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelBooking :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r, CoreMetrics m) => DRB.Booking -> m ()
cancelBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason booking.id
  DB.runTransaction $ do
    QRideB.updateStatus booking.id DRB.CANCELLED
    QBCR.upsert bookingCancellationReason
  Notify.notifyOnBookingCancelled booking DBCR.ByApplication
  where
    buildBookingCancellationReason bookingId = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            reasonStage = Nothing,
            additionalInfo = Nothing
          }
