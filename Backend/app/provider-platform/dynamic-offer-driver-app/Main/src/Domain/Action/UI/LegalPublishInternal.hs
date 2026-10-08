module Domain.Action.UI.LegalPublishInternal
  ( postInternalLegalPublish,
  )
where

import qualified API.Types.UI.LegalPublishInternal
import qualified Dashboard.Common as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PolicyAndComplianceDocument as CPCD
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD

postInternalLegalPublish ::
  ( Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    API.Types.UI.LegalPublishInternal.LegalPublishReq ->
    Environment.Flow API.Types.UI.LegalPublishInternal.LegalPublishResp
  )
postInternalLegalPublish mbApiKey req = do
  when (req.entityType == Common.CustomerLegal) $
    throwError (InvalidRequest "Driver app accepts only DriverLegal or FleetOwnerLegal, got CustomerLegal")
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity (ShortId req.merchantShortId) (Kernel.Prelude.read . toString $ req.operatingCity)
      >>= fromMaybeM (InvalidRequest $ "No operating city " <> req.operatingCity <> " for merchant " <> req.merchantShortId)
  let merchantId = merchantOperatingCity.merchantId
      merchantOperatingCityId = merchantOperatingCity.id
  verifyToken mbApiKey merchantId
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
      mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
      when (fromMaybe False (mbTransporterConfig >>= (.limitsConfig) >>= (.sendLegalPolicyUpdateEmail))) $ do
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
                entityType = req.entityType,
                batchId = batchId,
                pageOffset = 0
              }
      pure $ API.Types.UI.LegalPublishInternal.LegalPublishResp {id = Id docId, created = True}

verifyToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Id DM.Merchant -> Environment.Flow ()
verifyToken mbToken merchantId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == mbToken) $
    throwError (AuthBlocked "Invalid internal api key")
