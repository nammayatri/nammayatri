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
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

confirm ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl, "version" ::: DeploymentVersion],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Id DP.Person ->
  Id DQuote.Quote ->
  Maybe Payment.PaymentMethodId ->
  Maybe Bool ->
  m SConfirm.DConfirmRes
confirm personId quoteId paymentMethodId isAdvanceBookingEnabled = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  whenJust isAdvanceBookingEnabled $ \isAdvanceBookingEnabled' -> do
    QSR.updateAdvancedBookingEnabled (Just isAdvanceBookingEnabled') quote.requestId
  isLockAcquired <- SConfirm.tryInitTriggerLock quote.requestId
  unless isLockAcquired $ do
    throwError . InvalidRequest $ "Lock on searchRequestId:-" <> quote.requestId.getId <> " to create booking already acquired, can't create booking for quoteId:-" <> quoteId.getId
  SConfirm.confirm SConfirm.DConfirmReq {..}

-- cancel booking when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelBooking :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r) => DRB.Booking -> m ()
cancelBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason
  otherRelatedParties <- Notify.getAllOtherRelatedPartyPersons booking
  _ <- QRideB.updateStatus booking.id DRB.CANCELLED
  _ <- QBPL.makeAllInactiveByBookingId booking.id
  _ <- QBCR.upsert bookingCancellationReason
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:- " <> booking.providerId <> "and domain:- " <> show Context.MOBILITY)
  Notify.notifyOnBookingCancelled booking DBCR.ByApplication bppDetails Nothing otherRelatedParties
  where
    buildBookingCancellationReason = do
      now <- getCurrentTime
      return $
        DBCR.BookingCancellationReason
          { bookingId = booking.id,
            rideId = Nothing,
            merchantId = Just booking.merchantId,
            distanceUnit = booking.distanceUnit,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            reasonStage = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            riderId = Just booking.riderId,
            createdAt = now,
            updatedAt = now
          }
