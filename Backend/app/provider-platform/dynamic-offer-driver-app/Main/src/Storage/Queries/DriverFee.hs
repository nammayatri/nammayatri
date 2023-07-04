{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Person
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (MeshError (MKeyNotFound), MeshResult)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF

-- create :: DriverFee -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DriverFee -> m (MeshResult ())
create driverFee = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverFeeToBeam driverFee)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamDriverFeeToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamDF.id $ Se.Eq driverFeeId]
    Nothing -> pure Nothing

findByIdBeam :: L.MonadFlow m => Id DriverFee -> m (Maybe DriverFee)
findByIdBeam (Id driverFeeId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamDriverFeeToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamDF.id $ Se.Eq driverFeeId]
    Nothing -> pure Nothing

-- findByShortId :: Transactionable m => ShortId DriverFee -> m (Maybe DriverFee)
-- findByShortId shortId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $ driverFee ^. DriverFeeShortId ==. val (getShortId shortId)
--     return driverFee

findByShortId :: L.MonadFlow m => ShortId DriverFee -> m (Maybe DriverFee)
findByShortId shortId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamDriverFeeToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamDF.shortId $ Se.Eq $ getShortId shortId]
    Nothing -> pure Nothing

-- findPendingFeesByDriverFeeId :: Transactionable m => Id DriverFee -> m (Maybe DriverFee)
-- findPendingFeesByDriverFeeId driverFeeId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeId ==. val (getId driverFeeId)
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverFeeId :: L.MonadFlow m => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamDriverFeeToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDF.id $ Se.Eq driverFeeId, Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]]]
    Nothing -> pure Nothing

-- findPendingFeesByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findPendingFeesByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey (cast driverId))
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--     return driverFee

findPendingFeesByDriverId :: L.MonadFlow m => Id Driver -> m (Maybe DriverFee)
findPendingFeesByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamDriverFeeToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]]]
    Nothing -> pure Nothing

-- findLatestFeeByDriverId :: Transactionable m => Id Driver -> m (Maybe DriverFee)
-- findLatestFeeByDriverId driverId = do
--   findOne $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey $ cast driverId)
--     orderBy [desc $ driverFee ^. DriverFeeCreatedAt]
--     limit 1
--     return driverFee

findLatestFeeByDriverId :: L.MonadFlow m => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.Is BeamDF.driverId $ Se.Eq driverId] (Se.Desc BeamDF.createdAt) (Just 1) Nothing
      case res of
        Right (x : _) -> pure $ Just $ transformBeamDriverFeeToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

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

findOldestFeeByStatus :: L.MonadFlow m => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus (Id driverId) status = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq status]] (Se.Asc BeamDF.createdAt) (Just 1) Nothing
      case res of
        Right (x : _) -> pure $ Just $ transformBeamDriverFeeToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findFeesInRangeWithStatus :: Transactionable m => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
-- findFeesInRangeWithStatus startTime endTime status = do
--   findAll $ do
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeStartTime >=. val startTime
--         &&. driverFee ^. DriverFeeEndTime <=. val endTime
--         &&. driverFee ^. DriverFeeStatus ==. val status
--     return driverFee

findFeesInRangeWithStatus :: L.MonadFlow m => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
findFeesInRangeWithStatus startTime endTime status = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamDriverFeeToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime, Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime, Se.Is BeamDF.status $ Se.Eq status]]
    Nothing -> pure []

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

findWindowsWithStatus :: L.MonadFlow m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus (Id driverId) startTime endTime mbStatus limitVal offsetVal = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure []) (transformBeamDriverFeeToDomain <$>)
        <$> KV.findAllWithOptionsKVConnector
          dbConf'
          updatedMeshConfig
          [Se.And $ [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime, Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime] <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]]
          (Se.Asc BeamDF.createdAt)
          (Just limitVal)
          (Just offsetVal)
    Nothing -> pure []

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

findOngoingAfterEndTime :: L.MonadFlow m => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime (Id driverId) now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDF.driverId $ Se.Eq driverId, Se.Is BeamDF.status $ Se.Eq ONGOING, Se.Is BeamDF.endTime $ Se.LessThanOrEq now]]
      case res of
        Right (x : _) -> pure $ Just $ transformBeamDriverFeeToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findUnpaidAfterPayBy :: Transactionable m => Id Person -> UTCTime -> m (Maybe DriverFee)
-- findUnpaidAfterPayBy driverId now = do
--   findOne $ do
--     -- assuming only one such entry only
--     driverFee <- from $ table @DriverFeeT
--     where_ $
--       driverFee ^. DriverFeeDriverId ==. val (toKey driverId)
--         &&. driverFee ^. DriverFeeStatus `in_` valList [PAYMENT_PENDING, PAYMENT_OVERDUE]
--         &&. driverFee ^. DriverFeePayBy <=. val now
--     return driverFee

findUnpaidAfterPayBy :: L.MonadFlow m => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy (Id driverId) now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure Nothing) (transformBeamDriverFeeToDomain <$>)
        <$> KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamDF.driverId $ Se.Eq driverId,
                Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
                Se.Is BeamDF.payBy $ Se.LessThanOrEq now
              ]
          ]
    Nothing -> pure Nothing

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

updateFee :: L.MonadFlow m => Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> m (MeshResult ())
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
      dbConf <- L.getOption KBT.PsqlDbCfg
      let modelName = Se.modelTableName @BeamDF.DriverFeeT
      let updatedMeshConfig = setMeshConfig modelName
      case dbConf of
        Just dbConf' ->
          KV.updateWoReturningWithKVConnector
            dbConf'
            updatedMeshConfig
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
        Nothing -> pure (Left (MKeyNotFound "DB Config not found"))
    Nothing -> pure (Left (MKeyNotFound "Could not retrieve driver fee object"))

-- updateStatus :: DriverFeeStatus -> Id DriverFee -> UTCTime -> SqlDB ()
-- updateStatus status driverFeeId now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFeeStatus =. val status,
--         DriverFeeUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverFeeId ==. val (getId driverFeeId)
-- updateActivity :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> Maybe DriverMode -> m (MeshResult ())
-- updateActivity (Id driverId) isActive mode = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamDI.DriverInformationT
--   let updatedMeshConfig = setMeshConfig modelName
--   now <- getCurrentTime
--   case dbConf of
--     Just dbConf' ->
--       KV.updateWoReturningWithKVConnector
--         dbConf'
--         updatedMeshConfig
--         [ Se.Set BeamDI.active isActive,
--           Se.Set BeamDI.mode mode,
--           Se.Set BeamDI.updatedAt now
--         ]
--         [Se.Is BeamDI.driverId (Se.Eq driverId)]
--     Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateStatus :: L.MonadFlow m => DriverFeeStatus -> Id DriverFee -> UTCTime -> m (MeshResult ())
updateStatus status (Id driverFeeId) now = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDF.DriverFeeT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDF.status status,
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.id (Se.Eq driverFeeId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamDriverFeeToDomain :: BeamDF.DriverFee -> DriverFee
transformBeamDriverFeeToDomain BeamDF.DriverFeeT {..} = do
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

transformDomainDriverFeeToBeam :: DriverFee -> BeamDF.DriverFee
transformDomainDriverFeeToBeam DriverFee {..} =
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
