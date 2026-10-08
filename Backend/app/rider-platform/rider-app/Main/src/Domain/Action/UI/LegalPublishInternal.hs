module Domain.Action.UI.LegalPublishInternal
  ( postInternalLegalPublish,
  )
where

import qualified API.Types.UI.LegalPublishInternal
import qualified Dashboard.Common as Common
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PolicyAndComplianceDocument as CPCD
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD

postInternalLegalPublish ::
  ( Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    API.Types.UI.LegalPublishInternal.LegalPublishReq ->
    Environment.Flow API.Types.UI.LegalPublishInternal.LegalPublishResp
  )
postInternalLegalPublish mbApiKey req = do
  checkInternalApiKey mbApiKey
  unless (req.entityType == Common.CustomerLegal) $
    throwError (InvalidRequest $ "Rider app accepts only CustomerLegal, got " <> show req.entityType)
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity (ShortId req.merchantShortId) (Kernel.Prelude.read . toString $ req.operatingCity)
      >>= fromMaybeM (InvalidRequest $ "No operating city " <> req.operatingCity <> " for merchant " <> req.merchantShortId)
  let merchantId = merchantOperatingCity.merchantId
      merchantOperatingCityId = merchantOperatingCity.id
  existingDocs <- QPCD.findAllByMerchant merchantId
  let existing =
        Kernel.Prelude.find
          ( \d ->
              d.policyType == req.policyType
                && d.entityType == Just req.entityType
                && d.version == req.version
          )
          existingDocs
  case existing of
    Just row -> pure $ API.Types.UI.LegalPublishInternal.LegalPublishResp {id = row.id, created = False}
    Nothing -> do
      now <- getCurrentTime
      docId <- generateGUID
      let row =
            DPCD.PolicyAndComplianceDocument
              { id = Id docId,
                policyType = req.policyType,
                entityType = Just req.entityType,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId,
                version = req.version,
                url = req.url,
                isMandatory = req.isMandatory,
                enabled = req.enabled,
                metadata = req.metadata,
                effectiveDate = Just req.effectiveDate,
                createdAt = now,
                updatedAt = now
              }
      QPCD.create row
      CPCD.clearMerchantCache merchantId
      mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
      when (fromMaybe False (mbRiderConfig >>= (.sendLegalPolicyUpdateEmail))) $ do
        batchId <- generateGUID
        void $
          createJobIn @_ @'SendLegalPolicyNotification
            (Just merchantId)
            (Just merchantOperatingCityId)
            0
            SendLegalPolicyNotificationJobData
              { merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId,
                policyDocId = Id docId,
                batchId = batchId,
                pageOffset = 0
              }
      pure $ API.Types.UI.LegalPublishInternal.LegalPublishResp {id = Id docId, created = True}

checkInternalApiKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow ()
checkInternalApiKey mbKey = do
  expected <- asks (.internalAPIKey)
  unless (Just expected == mbKey) $
    throwError (AuthBlocked "Invalid internal API key")
