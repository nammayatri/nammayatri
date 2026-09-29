{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PolicyAndComplianceDocument (module Storage.Queries.PolicyAndComplianceDocument, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.PolicyAndComplianceDocument
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PolicyAndComplianceDocument as Beam
import Storage.Queries.PolicyAndComplianceDocumentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument] -> m ())
createMany = traverse_ create

findAllByMerchant :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument]))
findAllByMerchant merchantId = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

updateFields ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument -> m ())
updateFields url isMandatory enabled metadata id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.url url,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.metadata metadata,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument -> m (Maybe Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument -> m ())
updateByPrimaryKey (Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.metadata metadata,
      Se.Set Beam.policyType policyType,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.url url,
      Se.Set Beam.version version
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
