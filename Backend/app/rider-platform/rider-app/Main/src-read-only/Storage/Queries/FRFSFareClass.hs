{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSFareClass where

import qualified Domain.Types.FRFSFareClass
import qualified Domain.Types.FRFSVehicleServiceTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSFareClass as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFareClass.FRFSFareClass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSFareClass.FRFSFareClass] -> m ())
createMany = traverse_ create

findAllByVehicleServiceTierId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m ([Domain.Types.FRFSFareClass.FRFSFareClass]))
findAllByVehicleServiceTierId vehicleServiceTierId = do findAllWithKV [Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)]

findAllByVehicleServiceTierIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> Kernel.Prelude.Bool -> m ([Domain.Types.FRFSFareClass.FRFSFareClass]))
findAllByVehicleServiceTierIdAndEnabled vehicleServiceTierId enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

updateDisplayNameAndTimeBoundsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.TimeBound.TimeBound -> Kernel.Types.Id.Id Domain.Types.FRFSFareClass.FRFSFareClass -> m ())
updateDisplayNameAndTimeBoundsById displayName timeBounds id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.displayName displayName, Se.Set Beam.timeBounds timeBounds, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateEnabledById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSFareClass.FRFSFareClass -> m ())
updateEnabledById enabled id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFareClass.FRFSFareClass -> m (Maybe Domain.Types.FRFSFareClass.FRFSFareClass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFareClass.FRFSFareClass -> m ())
updateByPrimaryKey (Domain.Types.FRFSFareClass.FRFSFareClass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.classCode classCode,
      Se.Set Beam.displayName displayName,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.routeTag routeTag,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleServiceTierId (Kernel.Types.Id.getId vehicleServiceTierId),
      Se.Set Beam.vehicleTag vehicleTag,
      Se.Set Beam.vehicleVariant vehicleVariant,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSFareClass Domain.Types.FRFSFareClass.FRFSFareClass where
  fromTType' (Beam.FRFSFareClassT {..}) = do
    pure $
      Just
        Domain.Types.FRFSFareClass.FRFSFareClass
          { classCode = classCode,
            displayName = displayName,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            routeTag = routeTag,
            timeBounds = timeBounds,
            vehicleServiceTierId = Kernel.Types.Id.Id vehicleServiceTierId,
            vehicleTag = vehicleTag,
            vehicleVariant = vehicleVariant,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSFareClass Domain.Types.FRFSFareClass.FRFSFareClass where
  toTType' (Domain.Types.FRFSFareClass.FRFSFareClass {..}) = do
    Beam.FRFSFareClassT
      { Beam.classCode = classCode,
        Beam.displayName = displayName,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.routeTag = routeTag,
        Beam.timeBounds = timeBounds,
        Beam.vehicleServiceTierId = Kernel.Types.Id.getId vehicleServiceTierId,
        Beam.vehicleTag = vehicleTag,
        Beam.vehicleVariant = vehicleVariant,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
