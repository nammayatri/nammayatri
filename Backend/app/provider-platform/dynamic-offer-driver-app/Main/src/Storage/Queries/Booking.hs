{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Booking where

import Domain.Types.Booking
import Domain.Types.DriverQuote as DDQ
import Domain.Types.Merchant
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchTry as DST
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Storage.Hedis.Config (HedisEnv)
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP

createBooking :: MonadFlow m => Booking -> m ()
createBooking = createWithKV

create :: MonadFlow m => Booking -> m ()
create dBooking = QBBL.create dBooking.fromLocation >> QBBL.create dBooking.toLocation >> createBooking dBooking

findById :: MonadFlow m => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findBySTId :: MonadFlow m => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = do
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  maybe (pure Nothing) (\dQ -> findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq $ getId $ DDQ.id dQ]) mbDriverQuote

updateStatus :: MonadFlow m => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.status rbStatus, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderId :: MonadFlow m => Id Booking -> Id RiderDetails -> m ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.riderId $ Just $ getId riderId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderName :: MonadFlow m => Id Booking -> Text -> m ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamB.riderName $ Just riderName, Se.Set BeamB.updatedAt now] [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSpecialZoneOtpCode :: MonadFlow m => Id Booking -> Text -> m ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

findStuckBookings :: MonadFlow m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings (Id merchantId) bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60)) now
  (Domain.Types.Booking.id <$>)
    <$> findAllWithDb
      [ Se.And
          [ Se.Is BeamB.providerId (Se.Eq merchantId),
            Se.Is BeamB.id (Se.In (getId <$> bookingIds)),
            Se.Is BeamB.status (Se.In [NEW, TRIP_ASSIGNED]),
            Se.Is BeamB.createdAt (Se.LessThanOrEq updatedTimestamp)
          ]
      ]

findBookingBySpecialZoneOTP ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "cacheConfig" r CacheConfig,
    HasField "enablePrometheusMetricLogging" r Bool,
    HasField "enableRedisLatencyLogging" r Bool,
    HasField "esqDBEnv" r EsqDBEnv,
    HasField "hedisClusterEnv" r HedisEnv,
    HasField "hedisEnv" r HedisEnv,
    HasField "hedisMigrationStage" r Bool,
    HasField "hedisNonCriticalClusterEnv" r HedisEnv,
    HasField "hedisNonCriticalEnv" r HedisEnv,
    HasField "loggerEnv" r LoggerEnv
  ) =>
  Id Merchant ->
  Text ->
  UTCTime ->
  m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  transporterConfig <- TC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now transporterConfig.specialZoneBookingOtpExpiry
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: MonadFlow m => Id Merchant -> Text -> UTCTime -> Int -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP (Id merchantId) otpCode now bookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (fromIntegral (- (bookingOtpExpiry * 60)) :: NominalDiffTime) now
  (Domain.Types.Booking.id <$>) <$> findOneWithKV [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.providerId $ Se.Eq merchantId, Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

cancelBookings :: MonadFlow m => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [Se.Set BeamB.status CANCELLED, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

findFareForCancelledBookings :: MonadFlow m => [Id Booking] -> m Money
findFareForCancelledBookings bookingIds = findAllWithKV [Se.And [Se.Is BeamB.status $ Se.Eq CANCELLED, Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]] <&> sum . map Domain.Types.Booking.estimatedFare

instance FromTType' BeamB.Booking Booking where
  fromTType' BeamB.BookingT {..} = do
    fl <- QBBL.findById (Id fromLocationId) >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> show fromLocationId)
    tl <- QBBL.findById (Id toLocationId) >>= fromMaybeM (InternalError $ "ToLocation not found in booking for toLocationId: " <> show toLocationId)
    fp <- QueriesFP.findById (Id fareParametersId)
    pUrl <- parseBaseUrl bapUri
    if isJust fp
      then
        pure $
          Just
            Booking
              { id = Id id,
                transactionId = transactionId,
                quoteId = quoteId,
                status = status,
                bookingType = bookingType,
                specialLocationTag = specialLocationTag,
                specialZoneOtpCode = specialZoneOtpCode,
                disabilityTag = disabilityTag,
                area = area,
                providerId = Id providerId,
                primaryExophone = primaryExophone,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                startTime = startTime,
                riderId = Id <$> riderId,
                fromLocation = fl,
                toLocation = tl,
                vehicleVariant = vehicleVariant,
                estimatedDistance = estimatedDistance,
                maxEstimatedDistance = maxEstimatedDistance,
                estimatedFare = estimatedFare,
                estimatedDuration = estimatedDuration,
                fareParams = fromJust fp, -- This fromJust is safe because of the check above.
                paymentMethodId = Id <$> paymentMethodId,
                riderName = riderName,
                paymentUrl = paymentUrl,
                createdAt = createdAt,
                updatedAt = updatedAt
              }
      else do
        logError $ "FareParameters not found for booking: " <> show id
        pure Nothing

instance ToTType' BeamB.Booking Booking where
  toTType' Booking {..} =
    BeamB.BookingT
      { BeamB.id = getId id,
        BeamB.transactionId = transactionId,
        BeamB.quoteId = quoteId,
        BeamB.status = status,
        BeamB.bookingType = bookingType,
        BeamB.specialLocationTag = specialLocationTag,
        BeamB.disabilityTag = disabilityTag,
        BeamB.specialZoneOtpCode = specialZoneOtpCode,
        BeamB.area = area,
        BeamB.providerId = getId providerId,
        BeamB.primaryExophone = primaryExophone,
        BeamB.bapId = bapId,
        BeamB.bapUri = showBaseUrl bapUri,
        BeamB.startTime = startTime,
        BeamB.riderId = getId <$> riderId,
        BeamB.bapCity = bapCity,
        BeamB.bapCountry = bapCountry,
        BeamB.fromLocationId = getId fromLocation.id,
        BeamB.toLocationId = getId toLocation.id,
        BeamB.vehicleVariant = vehicleVariant,
        BeamB.estimatedDistance = estimatedDistance,
        BeamB.maxEstimatedDistance = maxEstimatedDistance,
        BeamB.estimatedFare = estimatedFare,
        BeamB.estimatedDuration = estimatedDuration,
        BeamB.fareParametersId = getId fareParams.id,
        BeamB.paymentMethodId = getId <$> paymentMethodId,
        BeamB.paymentUrl = paymentUrl,
        BeamB.riderName = riderName,
        BeamB.createdAt = createdAt,
        BeamB.updatedAt = updatedAt
      }
