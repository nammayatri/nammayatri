module Domain.Action.Dashboard.PolicyDocument
  ( postPolicyDocumentCreate,
    postPolicyDocumentUpdate,
    getPolicyDocumentList,
  )
where

import qualified API.Types.RiderPlatform.Management.PolicyDocument as Common
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
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCity.id,
            version = req.version,
            url = req.url,
            isMandatory = req.isMandatory,
            enabled = fromMaybe True req.enabled,
            metadata = req.metadata,
            createdAt = now,
            updatedAt = now
          }
  QPCD.create doc
  CPCD.clearMerchantCache merchant.id
  pure $
    Common.PolicyCreateResp
      { id = docId.getId,
        policyType = doc.policyType,
        version = doc.version
      }

postPolicyDocumentUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.PolicyUpdateReq ->
  Flow APISuccess
postPolicyDocumentUpdate merchantShortId _ policyDocIdText req = do
  let policyDocId = Id policyDocIdText :: Id DPCD.PolicyAndComplianceDocument
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  doc <- QPCD.findByPrimaryKey policyDocId >>= fromMaybeM (InvalidRequest $ "Policy document not found: " <> policyDocIdText)
  unless (doc.merchantId == merchant.id) $
    throwError (InvalidRequest "Policy document does not belong to this merchant")
  QPCD.updateFields
    (fromMaybe doc.url req.url)
    (fromMaybe doc.isMandatory req.isMandatory)
    (fromMaybe doc.enabled req.enabled)
    (req.metadata <|> doc.metadata)
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
  total <- QPCDX.countAllByMerchant merchant.id
  pure $
    Common.PolicyListMgmtResp
      { documents = map toMgmtResp docs,
        totalCount = total
      }

toMgmtResp :: DPCD.PolicyAndComplianceDocument -> Common.PolicyDocumentMgmtResp
toMgmtResp d =
  Common.PolicyDocumentMgmtResp
    { id = d.id.getId,
      policyType = d.policyType,
      version = d.version,
      url = d.url,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      createdAt = d.createdAt,
      updatedAt = d.updatedAt
    }
