module Storage.Queries.PolicyAndComplianceDocumentExtra where

import qualified Dashboard.Common as Common
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PolicyAndComplianceDocument as Beam
import Storage.Queries.OrphanInstances.PolicyAndComplianceDocument ()

findTopNEnabledByTypeAndMerchantAndEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Common.LegalEntityType ->
  Common.PolicyType ->
  Int ->
  m [DPCD.PolicyAndComplianceDocument]
findTopNEnabledByTypeAndMerchantAndEntity (Id merchantId) entityType policyType n = do
  rows <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.policyType $ Se.Eq policyType,
            Se.Is Beam.entityType $ Se.Eq (Just entityType),
            Se.Is Beam.merchantId $ Se.Eq merchantId,
            Se.Is Beam.enabled $ Se.Eq True
          ]
      ]
  pure $ take n (sortOn (Down . effectiveOrCreatedAt) rows)

findAllLatestEnabledByMerchantAndEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Common.LegalEntityType ->
  m [DPCD.PolicyAndComplianceDocument]
findAllLatestEnabledByMerchantAndEntity merchantId entityType = do
  allEnabled <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq merchantId.getId,
            Se.Is Beam.entityType $ Se.Eq (Just entityType),
            Se.Is Beam.enabled $ Se.Eq True
          ]
      ]
  pure $ dedupByType (sortOn (Down . effectiveOrCreatedAt) allEnabled)
  where
    dedupByType = go mempty
      where
        go _ [] = []
        go seen (d : ds)
          | d.policyType `elem` seen = go seen ds
          | otherwise = d : go (d.policyType : seen) ds

effectiveOrCreatedAt :: DPCD.PolicyAndComplianceDocument -> UTCTime
effectiveOrCreatedAt d = fromMaybe d.createdAt d.effectiveDate

findByMerchantPolicyTypeEntityTypeVersion ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Common.PolicyType ->
  Common.LegalEntityType ->
  Text ->
  m (Maybe DPCD.PolicyAndComplianceDocument)
findByMerchantPolicyTypeEntityTypeVersion (Id merchantId) policyType entityType version =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.policyType $ Se.Eq policyType,
          Se.Is Beam.entityType $ Se.Eq (Just entityType),
          Se.Is Beam.version $ Se.Eq version
        ]
    ]

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
