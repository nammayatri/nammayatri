{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonPTStats where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Domain.Types.PersonPTStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonPTStats as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonPTStats.PersonPTStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonPTStats.PersonPTStats] -> m ())
createMany = traverse_ create

findAllByStaticPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.PersonPTStats.PersonPTStats])
findAllByStaticPersonId staticPersonId = do findAllWithKV [Se.Is Beam.staticPersonId $ Se.Eq staticPersonId]

findByDimensions ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType -> Domain.Types.PersonPTStats.FRFSProductType -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> m (Maybe Domain.Types.PersonPTStats.PersonPTStats))
findByDimensions staticPersonId vehicleType vehicleServiceTierType productType passTypeId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.staticPersonId $ Se.Eq staticPersonId,
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.vehicleServiceTierType $ Se.Eq vehicleServiceTierType,
          Se.Is Beam.productType $ Se.Eq productType,
          Se.Is Beam.passTypeId $ Se.Eq (Kernel.Types.Id.getId <$> passTypeId)
        ]
    ]

updateCounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PersonPTStats.PersonPTStats -> m ())
updateCounts ticketCount purchaseCount lastPurchasedAt personId id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.ticketCount ticketCount,
      Se.Set Beam.purchaseCount purchaseCount,
      Se.Set Beam.lastPurchasedAt lastPurchasedAt,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePersonIdById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PersonPTStats.PersonPTStats -> m ())
updatePersonIdById personId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.personId (Kernel.Types.Id.getId personId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PersonPTStats.PersonPTStats -> m (Maybe Domain.Types.PersonPTStats.PersonPTStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonPTStats.PersonPTStats -> m ())
updateByPrimaryKey (Domain.Types.PersonPTStats.PersonPTStats {..}) = do
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

instance FromTType' Beam.PersonPTStats Domain.Types.PersonPTStats.PersonPTStats where
  fromTType' (Beam.PersonPTStatsT {..}) = do
    pure $
      Just
        Domain.Types.PersonPTStats.PersonPTStats
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lastPurchasedAt = lastPurchasedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            passTypeId = Kernel.Types.Id.Id <$> passTypeId,
            personId = Kernel.Types.Id.Id personId,
            productType = productType,
            purchaseCount = purchaseCount,
            staticPersonId = staticPersonId,
            ticketCount = ticketCount,
            updatedAt = updatedAt,
            vehicleServiceTierType = vehicleServiceTierType,
            vehicleType = vehicleType
          }

instance ToTType' Beam.PersonPTStats Domain.Types.PersonPTStats.PersonPTStats where
  toTType' (Domain.Types.PersonPTStats.PersonPTStats {..}) = do
    Beam.PersonPTStatsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastPurchasedAt = lastPurchasedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.passTypeId = Kernel.Types.Id.getId <$> passTypeId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.productType = productType,
        Beam.purchaseCount = purchaseCount,
        Beam.staticPersonId = staticPersonId,
        Beam.ticketCount = ticketCount,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vehicleType = vehicleType
      }
