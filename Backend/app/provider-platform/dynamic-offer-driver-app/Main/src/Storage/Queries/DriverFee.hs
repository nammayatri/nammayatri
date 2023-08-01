{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findAllWithOptionsKV, findAllWithOptionsKvInReplica, findOneWithKV, findOneWithKvInReplica, updateOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF

-- create :: DriverFee -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DriverFee -> m ()
create = createWithKV

-- findById :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findByIdInReplica (Id driverFeeId) = findOneWithKvInReplica [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findByIdBeam :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findByIdBeam (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

-- findByShortId :: Transactionable m => ShortId DriverFee -> m (Maybe DriverFee)
-- findByShortId shortId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $ driverFee ^. DriverFeeShortId ==. val (getShortId shortId)
--     return driverFee

findByShortId :: (L.MonadFlow m, Log m) => ShortId DriverFee -> m (Maybe DriverFee)
findByShortId shortId = findOneWithKV [Se.Is BeamDF.shortId $ Se.Eq $ getShortId shortId]

-- findPendingFeesByDriverFeeId :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findPendingFeesByDriverFeeId driverFeeId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeId ==. val (getId driverFeeId)
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverFeeId :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) = findOneWithKV [Se.And [Se.Is BeamDF.id $ Se.Eq driverFeeId, Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]]]

-- findPendingFeesByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findPendingFeesByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey (cast driverId))
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findPendingFeesByDriverId (Id driverId) = findOneWithKV [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]]]

-- findLatestFeeByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findLatestFeeByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
--     orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
--     limit 1
--     return driverFee

findLatestFeeByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId (Id driverId) = findAllWithOptionsKV [Se.Is BeamDF.driverId $ Se.Eq driverId] (Se.Desc BeamDF.createdAt) (Just 1) Nothing <&> listToMaybe

-- pure $ case res of
--   (x : _) -> Just x
--   _ -> Nothing

-- findOldestFeeByStatus :: Transactionable m => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
-- findOldestFeeByStatus driverId status = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
--         &&. driverFee ^. DriverFeeStatus ==. val status
--     orderBy [asc $ driverFee ^. DriverFeeCreatedAt]
--     limit 1
--     return driverFee

findOldestFeeByStatus :: (L.MonadFlow m, Log m) => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus (Id driverId) status = findAllWithOptionsKV [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq status]] (Se.Asc BeamDF.createdAt) (Just 1) Nothing <&> listToMaybe

findOldestFeeByStatusInReplica :: (L.MonadFlow m, Log m) => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatusInReplica (Id driverId) status = findAllWithOptionsKvInReplica [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq status]] (Se.Asc BeamDF.createdAt) (Just 1) Nothing <&> listToMaybe

-- findFeesInRangeWithStatus :: Transactionable m => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
-- findFeesInRangeWithStatus startTime endTime status = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeStartTime >=. val startTime
--         &&. driverFee ^. DriverFeeEndTime <=. val endTime
--         &&. driverFee ^. DriverFeeStatus ==. val status
--     return driverFee

findFeesInRangeWithStatus :: (L.MonadFlow m, Log m) => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
findFeesInRangeWithStatus startTime endTime status = findAllWithKV [Se.And [Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime, Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime, Se.Is BeamDF.status $ Se.Eq status]]

-- findWindowsWithStatus :: Transactionable m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
-- findWindowsWithStatus driverId startTime endTime mbStatus limitVal offsetVal = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--         &&. driverFee ^. DriverFeeStartTime >=. val startTime
--         &&. driverFee ^. DriverFeeEndTime <=. val endTime
--         &&. whenJust_ mbStatus (\status -> driverFee ^. DriverFeeStatus ==. val status)
--     limit $ fromIntegral limitVal
--     offset $ fromIntegral offsetVal
--     return driverFee

findWindowsWithStatus :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus (Id driverId) startTime endTime mbStatus limitVal offsetVal =
  findAllWithOptionsKV
    [Se.And $ [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime, Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime] <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findWindowsWithStatusInReplica :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatusInReplica (Id driverId) startTime endTime mbStatus limitVal offsetVal =
  findAllWithOptionsKvInReplica
    [Se.And $ [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime, Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime] <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

-- findOngoingAfterEndTime :: Transactionable m => Id Person -> UTCTime -> m (Maybe DriverFee)
-- findOngoingAfterEndTime driverId now = do
--   findOne $ do
--     -- assuming one such entry only
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--         &&. driverFee ^. DriverFeeStatus ==. val ONGOING
--         &&. driverFee ^. DriverFeeEndTime <=. val now
--     return driverFee

findOngoingAfterEndTime :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime (Id driverId) now = findOneWithKV [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq ONGOING, Se.Is BeamDF.endTime $ Se.LessThanOrEq now]]

findWindowsWithStatus :: Transactionable m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus driverId startTime endTime mbStatus limitVal offsetVal = do
  findAll $ do
    driverFee <- from $ table @DriverFeeT
    where_ $
      driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
        &&. driverFee ^. DriverFeeStartTime >=. val startTime
        &&. driverFee ^. DriverFeeEndTime <=. val endTime
        &&. whenJust_ mbStatus (\status -> driverFee ^. DriverFeeStatus ==. val status)
    orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return driverFee

findOngoingAfterEndTimeInReplica :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTimeInReplica (Id driverId) now = findOneWithKvInReplica [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq ONGOING, Se.Is BeamDF.endTime $ Se.LessThanOrEq now]]

findUnpaidAfterPayBy :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.payBy $ Se.LessThanOrEq now
        ]
    ]

-- updateFee :: Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> SqlDB ()
-- updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
--   let fare = fromMaybe 0 mbFare
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeGovtCharges +=. val govtCharges,
--         DriverFeePlatformFee +=. val platformFee,
--         DriverFeeCgst +=. val cgst,
--         DriverFeeSgst +=. val sgst,
--         DriverFeeStatus =. val ONGOING,
--         DriverFeeTotalEarnings +=. val fare,
--         DriverFeeNumRides +=. val 1, -- in the api, num_rides needed without cost contribution?
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateFee :: (L.MonadFlow m, Log m) => Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
  driverFeeObject <- findByIdBeam driverFeeId
  case driverFeeObject of
    Just df -> do
      let govtCharges' = df.govtCharges
      let platformFee' = df.platformFee.fee
      let cgst' = df.platformFee.cgst
      let sgst' = df.platformFee.sgst
      let totalEarnings = df.totalEarnings
      let numRides = df.numRides
      let fare = fromMaybe 0 mbFare
      updateOneWithKV
        [ Se.Set BeamDF.govtCharges $ govtCharges' + govtCharges,
          Se.Set BeamDF.platformFee $ platformFee' + platformFee,
          Se.Set BeamDF.cgst $ cgst' + cgst,
          Se.Set BeamDF.sgst $ sgst' + sgst,
          Se.Set BeamDF.status ONGOING,
          Se.Set BeamDF.totalEarnings $ totalEarnings + fare,
          Se.Set BeamDF.numRides $ numRides + 1, -- in the api, num_rides needed without cost contribution?
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()

-- updateStatus :: DriverFeeStatus -> Id DriverFee -> UTCTime -> SqlDB ()
-- updateStatus status driverFeeId now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeStatus =. val status,
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)

updateStatus :: (L.MonadFlow m, Log m) => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now =
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

instance FromTType' BeamDF.DriverFee DriverFee where
  fromTType' BeamDF.DriverFeeT {..} = do
    pure $
      Just
        DriverFee
          { id = Id id,
            shortId = ShortId shortId,
            driverId = Id driverId,
            govtCharges = govtCharges,
            platformFee = Domain.PlatformFee platformFee cgst sgst,
            numRides = numRides,
            payBy = payBy,
            totalEarnings = totalEarnings,
            startTime = startTime,
            endTime = endTime,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamDF.DriverFee DriverFee where
  toTType' DriverFee {..} = do
    BeamDF.DriverFeeT
      { BeamDF.id = getId id,
        BeamDF.shortId = getShortId shortId,
        BeamDF.driverId = getId driverId,
        BeamDF.govtCharges = govtCharges,
        BeamDF.platformFee = platformFee.fee,
        BeamDF.cgst = platformFee.cgst,
        BeamDF.sgst = platformFee.sgst,
        BeamDF.numRides = numRides,
        BeamDF.payBy = payBy,
        BeamDF.totalEarnings = totalEarnings,
        BeamDF.startTime = startTime,
        BeamDF.endTime = endTime,
        BeamDF.status = status,
        BeamDF.createdAt = createdAt,
        BeamDF.updatedAt = updatedAt
      }
