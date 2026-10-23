{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RCTransferNotice where

import qualified Domain.Types.Person
import qualified Domain.Types.RCTransferNotice
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RCTransferNotice as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCTransferNotice.RCTransferNotice -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RCTransferNotice.RCTransferNotice] -> m ())
createMany = traverse_ create

findAllByPreviousDriverIdAndAcknowledged ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m ([Domain.Types.RCTransferNotice.RCTransferNotice]))
findAllByPreviousDriverIdAndAcknowledged previousDriverId acknowledged = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.previousDriverId $ Se.Eq (Kernel.Types.Id.getId previousDriverId),
          Se.Is Beam.acknowledged $ Se.Eq acknowledged
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RCTransferNotice.RCTransferNotice -> m (Maybe Domain.Types.RCTransferNotice.RCTransferNotice))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAcknowledgedByPreviousDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAcknowledgedByPreviousDriverId acknowledged previousDriverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.acknowledged acknowledged, Se.Set Beam.updatedAt _now] [Se.Is Beam.previousDriverId $ Se.Eq (Kernel.Types.Id.getId previousDriverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RCTransferNotice.RCTransferNotice -> m (Maybe Domain.Types.RCTransferNotice.RCTransferNotice))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCTransferNotice.RCTransferNotice -> m ())
updateByPrimaryKey (Domain.Types.RCTransferNotice.RCTransferNotice {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acknowledged acknowledged,
      Se.Set Beam.hasOtherVehicles hasOtherVehicles,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.newDriverId (Kernel.Types.Id.getId newDriverId),
      Se.Set Beam.previousDriverId (Kernel.Types.Id.getId previousDriverId),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RCTransferNotice Domain.Types.RCTransferNotice.RCTransferNotice where
  fromTType' (Beam.RCTransferNoticeT {..}) = do
    pure $
      Just
        Domain.Types.RCTransferNotice.RCTransferNotice
          { acknowledged = acknowledged,
            createdAt = createdAt,
            hasOtherVehicles = hasOtherVehicles,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            newDriverId = Kernel.Types.Id.Id newDriverId,
            previousDriverId = Kernel.Types.Id.Id previousDriverId,
            rcId = Kernel.Types.Id.Id rcId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RCTransferNotice Domain.Types.RCTransferNotice.RCTransferNotice where
  toTType' (Domain.Types.RCTransferNotice.RCTransferNotice {..}) = do
    Beam.RCTransferNoticeT
      { Beam.acknowledged = acknowledged,
        Beam.createdAt = createdAt,
        Beam.hasOtherVehicles = hasOtherVehicles,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.newDriverId = Kernel.Types.Id.getId newDriverId,
        Beam.previousDriverId = Kernel.Types.Id.getId previousDriverId,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.updatedAt = updatedAt
      }
