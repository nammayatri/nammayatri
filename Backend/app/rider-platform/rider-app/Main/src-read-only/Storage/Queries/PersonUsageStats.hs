{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonUsageStats (module Storage.Queries.PersonUsageStats, module ReExport) where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Domain.Types.PersonUsageStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonUsageStats as Beam
import Storage.Queries.PersonUsageStatsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonUsageStats.PersonUsageStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonUsageStats.PersonUsageStats] -> m ())
createMany = traverse_ create

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.PersonUsageStats.PersonUsageStats]))
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findAllByStaticPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.PersonUsageStats.PersonUsageStats]))
findAllByStaticPersonId staticPersonId = do findAllWithKV [Se.Is Beam.staticPersonId $ Se.Eq staticPersonId]

findByDimensions ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType -> Domain.Types.PersonUsageStats.FRFSProductType -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> m (Maybe Domain.Types.PersonUsageStats.PersonUsageStats))
findByDimensions personId vehicleType vehicleServiceTierType productType passTypeId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.vehicleServiceTierType $ Se.Eq vehicleServiceTierType,
          Se.Is Beam.productType $ Se.Eq productType,
          Se.Is Beam.passTypeId $ Se.Eq (Kernel.Types.Id.getId <$> passTypeId)
        ]
    ]

updateCounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PersonUsageStats.PersonUsageStats -> m ())
updateCounts ticketCount purchaseCount lastPurchasedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.ticketCount ticketCount,
      Se.Set Beam.purchaseCount purchaseCount,
      Se.Set Beam.lastPurchasedAt lastPurchasedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePersonIdById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PersonUsageStats.PersonUsageStats -> m ())
updatePersonIdById personId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.personId (Kernel.Types.Id.getId personId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PersonUsageStats.PersonUsageStats -> m (Maybe Domain.Types.PersonUsageStats.PersonUsageStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonUsageStats.PersonUsageStats -> m ())
updateByPrimaryKey (Domain.Types.PersonUsageStats.PersonUsageStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.lastPurchasedAt lastPurchasedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.passTypeId (Kernel.Types.Id.getId <$> passTypeId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.productType productType,
      Se.Set Beam.purchaseCount purchaseCount,
      Se.Set Beam.staticPersonId staticPersonId,
      Se.Set Beam.ticketCount ticketCount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.vehicleType vehicleType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
