module Storage.Queries.PolicyAndComplianceDocumentExtra where

import qualified Dashboard.Common as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PolicyAndComplianceDocument as Beam
import Storage.Queries.OrphanInstances.PolicyAndComplianceDocument ()

findTopNEnabledByTypeAndMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Common.PolicyType ->
  Int ->
  m [DPCD.PolicyAndComplianceDocument]
findTopNEnabledByTypeAndMerchant (Id merchantId) policyType n =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.policyType $ Se.Eq policyType,
          Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.enabled $ Se.Eq True
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just n)
    Nothing

findAllLatestEnabledByMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  m [DPCD.PolicyAndComplianceDocument]
findAllLatestEnabledByMerchant merchantId = do
  allEnabled <-
    findAllWithOptionsKV
      [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId.getId, Se.Is Beam.enabled $ Se.Eq True]]
      (Se.Desc Beam.createdAt)
      Nothing
      Nothing
  pure $ dedupByType allEnabled
  where
    dedupByType = go mempty
      where
        go _ [] = []
        go seen (d : ds)
          | d.policyType `elem` seen = go seen ds
          | otherwise = d : go (d.policyType : seen) ds

findAllByMerchantPaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  m [DPCD.PolicyAndComplianceDocument]
findAllByMerchantPaginated (Id merchantId) mbLimit mbOffset =
  findAllWithOptionsKV
    [Se.Is Beam.merchantId $ Se.Eq merchantId]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset
