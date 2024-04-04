{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Exophone (module Storage.Queries.Exophone, module ReExport) where

import qualified Domain.Types.Exophone
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Exophone as Beam
import Storage.Queries.ExophoneExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.Exophone.Exophone -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Exophone.Exophone] -> m ())
createMany = traverse_ create

deleteByMerchantOpCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m ())
deleteByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do deleteWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findAllByMerchantOpCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.Exophone.Exophone])
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByMerchantOpCityIdServiceAndExophoneType ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Call.Types.CallService -> Domain.Types.Exophone.ExophoneType -> m [Domain.Types.Exophone.Exophone])
findByMerchantOpCityIdServiceAndExophoneType (Kernel.Types.Id.Id merchantOperatingCityId) callService exophoneType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.callService $ Se.Eq callService,
          Se.Is Beam.exophoneType $ Se.Eq exophoneType
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Exophone.Exophone -> m (Maybe Domain.Types.Exophone.Exophone))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Exophone.Exophone -> m ())
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
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
