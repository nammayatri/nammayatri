{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Data.Text (pack)
import Domain.Types.Booking
import Domain.Types.DriverQuote (DriverQuote)
import Domain.Types.Geometry (Geometry)
import Domain.Types.Merchant
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchRequest as DSR
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Id
import Kernel.Types.Time
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverQuote as DriverQuote
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Geometry (EntityField (..), GeometryT)

baseBookingTable ::
  From
    ( Table BookingT
        :& Table BookingLocationT
        :& Table BookingLocationT
        :& Table Fare.FareParametersT
    )
baseBookingTable =
  table @BookingT
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& loc1) -> rb ^. BookingFromLocationId ==. loc1 ^. BookingLocationTId)
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& _ :& loc2) -> rb ^. BookingToLocationId ==. loc2 ^. BookingLocationTId)
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& _ :& _ :& farePars) ->
                   rb ^. BookingFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

-- fareParams already created with driverQuote
create :: Booking -> SqlDB ()
create dBooking =
  withFullEntity dBooking $ \(booking, fromLoc, toLoc, _fareParams) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' booking

-- findById :: Transactionable m => Id Booking -> m (Maybe Booking)
-- findById bookingId = buildDType $ do
--   res <-
--     Esq.findOne' $ do
--       rb <- from $ table @BookingT
--       where_ $ rb ^. BookingTId ==. val (toKey bookingId)
--       pure rb
--   join <$> mapM buildFullBooking res

findById :: L.MonadFlow m => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamB.id $ Se.Eq bookingId]
      case result of
        Right b -> traverse transformBeamBookingToDomain b
        Left _ -> pure Nothing
    Nothing -> pure Nothing

findBySearchReq :: (Transactionable m) => Id DSR.SearchRequest -> m (Maybe Booking)
findBySearchReq searchReqId = buildDType $ do
  mbDriverQuoteT <- QDQuote.findDriverQuoteBySearchId searchReqId
  let mbDriverQuoteId = Id . DriverQuote.id <$> mbDriverQuoteT
  mbBookingT <- (join <$>) $ mapM findBookingByDriverQuoteId' mbDriverQuoteId

  join <$> mapM buildFullBooking mbBookingT

findBookingByDriverQuoteId' :: Transactionable m => Id DriverQuote -> DTypeBuilder m (Maybe BookingT)
findBookingByDriverQuoteId' driverQuoteId = Esq.findOne' $ do
  booking <- from $ table @BookingT
  where_ $ booking ^. BookingQuoteId ==. val driverQuoteId.getId
  pure booking

-- updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
-- updateStatus rbId rbStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingStatus =. val rbStatus,
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Booking -> BookingStatus -> m (MeshResult ())
updateStatus rbId rbStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamB.status rbStatus,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId rbId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateRiderId :: Id Booking -> Id RiderDetails -> SqlDB ()
-- updateRiderId rbId riderId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingRiderId =. val (Just $ toKey riderId),
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderId :: (L.MonadFlow m, MonadTime m) => Id Booking -> Id RiderDetails -> m (MeshResult ())
updateRiderId rbId riderId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamB.riderId $ Just $ getId riderId,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId rbId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateRiderName :: Id Booking -> Text -> SqlDB ()
-- updateRiderName bookingId riderName = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingRiderName =. val (Just riderName),
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateRiderName :: (L.MonadFlow m, MonadTime m) => Id Booking -> Text -> m (MeshResult ())
updateRiderName bookingId riderName = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamB.riderName $ Just riderName,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateSpecialZoneOtpCode :: Id Booking -> Text -> SqlDB ()
-- updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingSpecialZoneOtpCode =. val (Just specialZoneOtpCode),
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateSpecialZoneOtpCode :: (L.MonadFlow m, MonadTime m) => Id Booking -> Text -> m (MeshResult ())
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

findBookingBySpecialZoneOTP :: (L.MonadFlow m, Transactionable m) => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP merchantId otpCode now = do
  Esq.findOne $ do
    booking <- from $ table @BookingT
    let otpExpiryCondition =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.MINUTE 30] >=. val now
    where_ $
      booking ^. BookingSpecialZoneOtpCode ==. val (Just otpCode)
        &&. (booking ^. BookingStatus ==. val NEW &&. otpExpiryCondition)
        &&. booking ^. BookingProviderId ==. val (toKey merchantId)
    pure $ booking ^. BookingTId

-- cancelBookings :: [Id Booking] -> UTCTime -> SqlDB ()
-- cancelBookings bookingIds now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingStatus =. val CANCELLED,
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)

cancelBookings :: L.MonadFlow m => [Id Booking] -> UTCTime -> m (MeshResult ())
cancelBookings bookingIds now = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamB.status CANCELLED,
          Se.Set BeamB.updatedAt now
        ]
        [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findAllBookings :: Transactionable m => m [Id Geometry]
findAllBookings = do
  Esq.findAll $ do
    booking <- from $ table @GeometryT
    pure $ booking ^. GeometryTId

transformBeamBookingToDomain :: L.MonadFlow m => BeamB.Booking -> m Booking
transformBeamBookingToDomain BeamB.BookingT {..} = do
  fl <- QBBL.findById (Id fromLocationId)
  tl <- QBBL.findById (Id toLocationId)
  fp <- QueriesFP.findById (Id fareParametersId)
  pUrl <- parseBaseUrl bapUri
  pure
    Booking
      { id = Id id,
        transactionId = transactionId,
        quoteId = quoteId,
        status = status,
        bookingType = bookingType,
        specialZoneOtpCode = specialZoneOtpCode,
        providerId = Id providerId,
        primaryExophone = primaryExophone,
        bapId = bapId,
        bapUri = pUrl,
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
        riderName = riderName,
        createdAt = createdAt,
        updatedAt = updatedAt
      }

transformDomainBookingToBeam :: Booking -> BeamB.Booking
transformDomainBookingToBeam Booking {..} =
  BeamB.BookingT
    { BeamB.id = getId id,
      BeamB.transactionId = transactionId,
      BeamB.quoteId = quoteId,
      BeamB.status = status,
      BeamB.bookingType = bookingType,
      BeamB.specialZoneOtpCode = specialZoneOtpCode,
      BeamB.providerId = getId providerId,
      BeamB.primaryExophone = primaryExophone,
      BeamB.bapId = bapId,
      BeamB.bapUri = (pack $ show bapUri),
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
      BeamB.riderName = riderName,
      BeamB.createdAt = createdAt,
      BeamB.updatedAt = updatedAt
    }
