{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverModuleCompletion where

import qualified Domain.Types.DriverModuleCompletion
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverModuleCompletion as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverModuleCompletion.DriverModuleCompletion] -> m ())
createMany = traverse_ create

findAllByDriverIdAndModuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m [Domain.Types.DriverModuleCompletion.DriverModuleCompletion])
findAllByDriverIdAndModuleId driverId moduleId = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.DriverModuleCompletion.DriverModuleCompletion])
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByDriverIdAndModuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.DriverModuleCompletion.DriverModuleCompletion))
findByDriverIdAndModuleId driverId moduleId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]]

findByDriverIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DriverModuleCompletion.ModuleCompletionStatus -> m [Domain.Types.DriverModuleCompletion.DriverModuleCompletion])
findByDriverIdAndStatus driverId status = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.Eq status]]

updateEntitiesCompleted ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> [Domain.Types.DriverModuleCompletion.ModuleCompletionEntity] -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m ())
updateEntitiesCompleted completedAt entitiesCompleted completionId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.completedAt completedAt,
      Se.Set Beam.entitiesCompleted entitiesCompleted,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId)]

updateExpiryTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m ())
updateExpiryTime expiry completionId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.expiry expiry, Se.Set Beam.updatedAt _now] [Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId)]

updatedCompletedAt ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> [Domain.Types.DriverModuleCompletion.ModuleCompletionEntity] -> Domain.Types.DriverModuleCompletion.ModuleCompletionStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m ())
updatedCompletedAt completedAt entitiesCompleted status ratingAtTheTimeOfCompletion completionId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.completedAt completedAt,
      Se.Set Beam.entitiesCompleted entitiesCompleted,
      Se.Set Beam.status status,
      Se.Set Beam.ratingAtTheTimeOfCompletion ratingAtTheTimeOfCompletion,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m (Maybe Domain.Types.DriverModuleCompletion.DriverModuleCompletion))
findByPrimaryKey completionId = do findOneWithKV [Se.And [Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m ())
updateByPrimaryKey (Domain.Types.DriverModuleCompletion.DriverModuleCompletion {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.completedAt completedAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.entitiesCompleted entitiesCompleted,
      Se.Set Beam.expiry expiry,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.moduleId (Kernel.Types.Id.getId moduleId),
      Se.Set Beam.ratingAtTheTimeOfCompletion ratingAtTheTimeOfCompletion,
      Se.Set Beam.startedAt startedAt,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId)]]

instance FromTType' Beam.DriverModuleCompletion Domain.Types.DriverModuleCompletion.DriverModuleCompletion where
  fromTType' (Beam.DriverModuleCompletionT {..}) = do
    pure $
      Just
        Domain.Types.DriverModuleCompletion.DriverModuleCompletion
          { completedAt = completedAt,
            completionId = Kernel.Types.Id.Id completionId,
            driverId = Kernel.Types.Id.Id driverId,
            entitiesCompleted = entitiesCompleted,
            expiry = expiry,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            moduleId = Kernel.Types.Id.Id moduleId,
            ratingAtTheTimeOfCompletion = ratingAtTheTimeOfCompletion,
            startedAt = startedAt,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverModuleCompletion Domain.Types.DriverModuleCompletion.DriverModuleCompletion where
  toTType' (Domain.Types.DriverModuleCompletion.DriverModuleCompletion {..}) = do
    Beam.DriverModuleCompletionT
      { Beam.completedAt = completedAt,
        Beam.completionId = Kernel.Types.Id.getId completionId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.entitiesCompleted = entitiesCompleted,
        Beam.expiry = expiry,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.ratingAtTheTimeOfCompletion = ratingAtTheTimeOfCompletion,
        Beam.startedAt = startedAt,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
