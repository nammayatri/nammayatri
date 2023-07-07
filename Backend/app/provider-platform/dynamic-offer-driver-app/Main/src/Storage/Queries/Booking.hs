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
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findOneWithKV, getMasterDBConfig', setMeshConfig, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP

createBooking :: L.MonadFlow m => Booking -> m (MeshResult ())
createBooking booking = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainBookingToBeam booking)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

createBooking' :: (L.MonadFlow m, Log m) => Booking -> m ()
createBooking' booking = do
  dbConf <- getMasterDBConfig'
  createWithKV dbConf booking

create :: L.MonadFlow m => Booking -> m (MeshResult ())
create dBooking = do
  _ <- QBBL.create dBooking.fromLocation
  _ <- QBBL.create dBooking.toLocation
  createBooking dBooking

findById :: L.MonadFlow m => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamB.id $ Se.Eq bookingId]
      case result of
        Right (Just result') -> transformBeamBookingToDomain result'
        _ -> pure Nothing
    Nothing -> pure Nothing

findById' :: (L.MonadFlow m, Log m) => Id Booking -> m (Maybe Booking)
findById' (Id bookingId) = do
  dbConf <- getMasterDBConfig'
  findOneWithKV dbConf [Se.Is BeamB.id $ Se.Eq bookingId]

findBySTId :: L.MonadFlow m => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = do
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  case mbDriverQuote of
    Nothing -> pure Nothing
    Just mbDriverQuote' -> do
      let quoteId = DDQ.id mbDriverQuote'
      dbConf <- L.getOption KBT.PsqlDbCfg
      let modelName = Se.modelTableName @BeamB.BookingT
      let updatedMeshConfig = setMeshConfig modelName
      case dbConf of
        Just dbCOnf' -> do
          result <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamB.quoteId $ Se.Eq $ getId quoteId]
          case result of
            Right (Just booking) -> transformBeamBookingToDomain booking
            _ -> pure Nothing
        Nothing -> pure Nothing

findBySTId' :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m (Maybe Booking)
findBySTId' searchTryId = do
  dbConf <- getMasterDBConfig'
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  maybe (pure Nothing) (\dQ -> findOneWithKV dbConf [Se.Is BeamB.quoteId $ Se.Eq $ getId $ DDQ.id dQ]) mbDriverQuote

-- case mbDriverQuote of
--   Nothing -> pure Nothing
--   Just mbDriverQuote' -> do
--     let quoteId = DDQ.id mbDriverQuote'
--     findOneWithKV dbConf [Se.Is BeamB.quoteId $ Se.Eq $ getId quoteId]

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Booking -> BookingStatus -> m (MeshResult ())
updateStatus rbId rbStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamB.status rbStatus,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId rbId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateRiderId :: (L.MonadFlow m, MonadTime m) => Id Booking -> Id RiderDetails -> m (MeshResult ())
updateRiderId rbId riderId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamB.riderId $ Just $ getId riderId,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId rbId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateRiderName :: (L.MonadFlow m, MonadTime m) => Id Booking -> Text -> m ()
updateRiderName bookingId riderName = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamB.riderName $ Just riderName,
            Se.Set BeamB.updatedAt now
          ]
          [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
    Nothing -> pure ()

updateRiderName' :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> Text -> m ()
updateRiderName' bookingId riderName = do
  dbConf <- getMasterDBConfig'
  now <- getCurrentTime
  updateWithKV dbConf [Se.Set BeamB.riderName $ Just riderName, Se.Set BeamB.updatedAt now] [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSpecialZoneOtpCode :: (L.MonadFlow m, MonadTime m) => Id Booking -> Text -> m (MeshResult ())
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findStuckBookings' :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings' (Id merchantId) bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  dbConf <- getMasterDBConfig'
  bookings <-
    findAllWithKV
      dbConf
      [ Se.And
          [ Se.Is BeamB.providerId $ Se.Eq merchantId,
            Se.Is BeamB.id (Se.In $ getId <$> bookingIds),
            Se.Is BeamB.status $ Se.In [NEW, TRIP_ASSIGNED],
            Se.Is BeamB.createdAt $ Se.LessThanOrEq updatedTimestamp
          ]
      ]
  pure $ Domain.Types.Booking.id <$> bookings

--   case result of
--     Right booking -> do
--       bookings <- mapM transformBeamBookingToDomain booking
--       pure $ Domain.Types.Booking.id <$> catMaybes bookings
--     _ -> pure []
-- Nothing -> pure []

findStuckBookings :: (L.MonadFlow m, MonadTime m) => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings (Id merchantId) bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamB.providerId $ Se.Eq merchantId,
                Se.Is BeamB.id (Se.In $ getId <$> bookingIds),
                Se.Is BeamB.status $ Se.In [NEW, TRIP_ASSIGNED],
                Se.Is BeamB.createdAt $ Se.LessThanOrEq updatedTimestamp
              ]
          ]
      case result of
        Right booking -> do
          bookings <- mapM transformBeamBookingToDomain booking
          pure $ Domain.Types.Booking.id <$> catMaybes bookings
        _ -> pure []
    Nothing -> pure []

findBookingBySpecialZoneOTP :: L.MonadFlow m => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: L.MonadFlow m => Id Merchant -> Text -> UTCTime -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP (Id merchantId) otpCode now = do
  let otpExpiryCondition = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.providerId $ Se.Eq merchantId, Se.Is BeamB.createdAt $ Se.LessThanOrEq otpExpiryCondition]]
      case result of
        Right (Just booking) -> do
          bookingId <- transformBeamBookingToDomain booking
          pure $ Domain.Types.Booking.id <$> bookingId
        _ -> pure Nothing
    Nothing -> pure Nothing

cancelBookings :: L.MonadFlow m => [Id Booking] -> UTCTime -> m (MeshResult ())
cancelBookings bookingIds now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamB.status CANCELLED,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamBookingToDomain :: L.MonadFlow m => BeamB.Booking -> m (Maybe Booking)
transformBeamBookingToDomain BeamB.BookingT {..} = do
  fl <- QBBL.findById (Id fromLocationId)
  tl <- QBBL.findById (Id toLocationId)
  fp <- QueriesFP.findById (Id fareParametersId)
  pUrl <- parseBaseUrl bapUri
  if isJust fl && isJust tl && isJust fp
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
              area = area,
              providerId = Id providerId,
              primaryExophone = primaryExophone,
              bapId = bapId,
              bapUri = pUrl,
              bapCity = bapCity,
              bapCountry = bapCountry,
              startTime = startTime,
              riderId = Id <$> riderId,
              fromLocation = fromJust fl,
              toLocation = fromJust tl,
              vehicleVariant = vehicleVariant,
              estimatedDistance = estimatedDistance,
              maxEstimatedDistance = maxEstimatedDistance,
              estimatedFare = estimatedFare,
              estimatedDuration = estimatedDuration,
              fareParams = fromJust fp,
              paymentMethodId = Id <$> paymentMethodId,
              riderName = riderName,
              createdAt = createdAt,
              updatedAt = updatedAt
            }
    else pure Nothing

instance FromTType' BeamB.Booking Booking where
  fromTType' BeamB.BookingT {..} = do
    fl <- QBBL.findById (Id fromLocationId)
    tl <- QBBL.findById (Id toLocationId)
    fp <- QueriesFP.findById (Id fareParametersId)
    pUrl <- parseBaseUrl bapUri
    if isJust fl && isJust tl && isJust fp
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
                area = area,
                providerId = Id providerId,
                primaryExophone = primaryExophone,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                startTime = startTime,
                riderId = Id <$> riderId,
                fromLocation = fromJust fl,
                toLocation = fromJust tl,
                vehicleVariant = vehicleVariant,
                estimatedDistance = estimatedDistance,
                maxEstimatedDistance = maxEstimatedDistance,
                estimatedFare = estimatedFare,
                estimatedDuration = estimatedDuration,
                fareParams = fromJust fp,
                paymentMethodId = Id <$> paymentMethodId,
                riderName = riderName,
                createdAt = createdAt,
                updatedAt = updatedAt
              }
      else pure Nothing

transformDomainBookingToBeam :: Booking -> BeamB.Booking
transformDomainBookingToBeam Booking {..} =
  BeamB.BookingT
    { BeamB.id = getId id,
      BeamB.transactionId = transactionId,
      BeamB.quoteId = quoteId,
      BeamB.status = status,
      BeamB.bookingType = bookingType,
      BeamB.specialLocationTag = specialLocationTag,
      BeamB.specialZoneOtpCode = specialZoneOtpCode,
      BeamB.area = area,
      BeamB.providerId = getId providerId,
      BeamB.primaryExophone = primaryExophone,
      BeamB.bapId = bapId,
      BeamB.bapUri = showBaseUrl bapUri,
      BeamB.bapCity = bapCity,
      BeamB.bapCountry = bapCountry,
      BeamB.startTime = startTime,
      BeamB.riderId = getId <$> riderId,
      BeamB.fromLocationId = getId fromLocation.id,
      BeamB.toLocationId = getId toLocation.id,
      BeamB.vehicleVariant = vehicleVariant,
      BeamB.estimatedDistance = estimatedDistance,
      BeamB.maxEstimatedDistance = maxEstimatedDistance,
      BeamB.estimatedFare = estimatedFare,
      BeamB.estimatedDuration = estimatedDuration,
      BeamB.fareParametersId = getId fareParams.id,
      BeamB.paymentMethodId = getId <$> paymentMethodId,
      BeamB.riderName = riderName,
      BeamB.createdAt = createdAt,
      BeamB.updatedAt = updatedAt
    }

instance ToTType' BeamB.Booking Booking where
  toTType' Booking {..} =
    BeamB.BookingT
      { BeamB.id = getId id,
        BeamB.transactionId = transactionId,
        BeamB.quoteId = quoteId,
        BeamB.status = status,
        BeamB.bookingType = bookingType,
        BeamB.specialLocationTag = specialLocationTag,
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
        BeamB.riderName = riderName,
        BeamB.createdAt = createdAt,
        BeamB.updatedAt = updatedAt
      }
