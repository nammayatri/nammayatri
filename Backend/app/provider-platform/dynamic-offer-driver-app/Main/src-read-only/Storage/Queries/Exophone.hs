{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Exophone (module Storage.Queries.Exophone, module ReExport) where

import qualified Domain.Types.Exophone
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Exophone as Beam
import Storage.Queries.ExophoneExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Exophone.Exophone -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Exophone.Exophone] -> m ())
createMany = traverse_ create

deleteByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
deleteByMerchantOpCityId merchantOperatingCityId = do deleteWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.Exophone.Exophone])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByMerchantOpCityIdServiceAndExophoneType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Call.Types.CallService -> Domain.Types.Exophone.ExophoneType -> m [Domain.Types.Exophone.Exophone])
findByMerchantOpCityIdServiceAndExophoneType merchantOperatingCityId callService exophoneType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.callService $ Se.Eq callService,
          Se.Is Beam.exophoneType $ Se.Eq exophoneType
        ]
    ]

findByMerchantOpCityIdAndExophoneType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Exophone.ExophoneType -> m [Domain.Types.Exophone.Exophone])
findByMerchantOpCityIdAndExophoneType merchantOperatingCityId exophoneType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.exophoneType $ Se.Eq exophoneType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Exophone.Exophone -> m (Maybe Domain.Types.Exophone.Exophone))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Exophone.Exophone -> m ())
updateByPrimaryKey (Domain.Types.Exophone.Exophone {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backupPhone backupPhone,
      Se.Set Beam.callService callService,
      Se.Set Beam.exophoneType exophoneType,
      Se.Set Beam.isPrimaryDown isPrimaryDown,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.primaryPhone primaryPhone,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
