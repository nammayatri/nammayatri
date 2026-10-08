module Domain.Action.Dashboard.Management.PolicyDocument
  ( postPolicyDocumentCreate,
    postPolicyDocumentUpdate,
    getPolicyDocumentList,
  )
where

import qualified API.Types.ProviderPlatform.Management.PolicyDocument as Common
import qualified Dashboard.Common as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PolicyAndComplianceDocument as CPCD
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD
import qualified Storage.Queries.PolicyAndComplianceDocumentExtra as QPCDX

postPolicyDocumentCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.PolicyCreateReq ->
  Flow Common.PolicyCreateResp
postPolicyDocumentCreate merchantShortId opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  now <- getCurrentTime
  docId <- generateGUID
  let doc =
        DPCD.PolicyAndComplianceDocument
          { id = docId,
            policyType = req.policyType,
            entityType = req.entityType,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCity.id,
            version = req.version,
            url = req.url,
            isMandatory = req.isMandatory,
            enabled = fromMaybe True req.enabled,
            metadata = req.metadata,
            effectiveDate = req.effectiveDate,
            createdAt = now,
            updatedAt = now
          }
  QPCD.create doc
  CPCD.clearMerchantCache merchant.id
  -- TODO (compliance email): send publish notification here. Reuse HtmlType +
  -- existing email infra; recipient list hardcoded for yearly cadence.
  pure $
    Common.PolicyCreateResp
      { id = docId.getId,
        policyType = doc.policyType,
        entityType = doc.entityType,
        version = doc.version
      }

postPolicyDocumentUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.PolicyAndComplianceDocument ->
  Common.PolicyUpdateReq ->
  Flow APISuccess
postPolicyDocumentUpdate merchantShortId _ policyDocIdCommon req = do
  let policyDocId = cast @Common.PolicyAndComplianceDocument @DPCD.PolicyAndComplianceDocument policyDocIdCommon
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  doc <- QPCD.findByPrimaryKey policyDocId >>= fromMaybeM (InvalidRequest $ "Policy document not found: " <> policyDocId.getId)
  unless (doc.merchantId == merchant.id) $
    throwError (InvalidRequest "Policy document does not belong to this merchant")
  QPCD.updateFields
    (fromMaybe doc.url req.url)
    (fromMaybe doc.isMandatory req.isMandatory)
    (fromMaybe doc.enabled req.enabled)
    (req.metadata <|> doc.metadata)
    (req.effectiveDate <|> doc.effectiveDate)
    policyDocId
  CPCD.clearMerchantCache merchant.id
  pure Success

getPolicyDocumentList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.PolicyListMgmtResp
getPolicyDocumentList merchantShortId _ mbLimit mbOffset = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  docs <- QPCDX.findAllByMerchantPaginated merchant.id mbLimit mbOffset
  pure $ Common.PolicyListMgmtResp {documents = map toMgmtResp docs}

toMgmtResp :: DPCD.PolicyAndComplianceDocument -> Common.PolicyDocumentMgmtResp
toMgmtResp d =
  Common.PolicyDocumentMgmtResp
    { id = d.id.getId,
      policyType = d.policyType,
      entityType = d.entityType,
      version = d.version,
      url = d.url,
      effectiveDate = d.effectiveDate,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      createdAt = d.createdAt,
      updatedAt = d.updatedAt
    }
