{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateEntity (module Storage.Queries.CorporateEntity, module ReExport) where

import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateEntity as Beam
import Storage.Queries.CorporateEntityExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateEntity ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateEntity.CorporateEntity -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateEntity.CorporateEntity] -> m ())
createMany = traverse_ create

findByMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.CorporateEntity.CorporateEntity])
findByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findByStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Domain.Types.CorporateEntity.CorporateEntityStatus -> m [Domain.Types.CorporateEntity.CorporateEntity])
findByStatus merchantId status = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.status $ Se.Eq (Kernel.Prelude.show status)]]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateEntity.CorporateEntityStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m ())
updateStatus status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status (Kernel.Prelude.show status),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m (Maybe Domain.Types.CorporateEntity.CorporateEntity))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
