module Domain.Action.UI.PolicyDocument
  ( getPolicyLatest,
    getPolicyList,
    postPolicyAccept,
    isGoOnlineBlockerActive,
    findBlockingUnacceptedPolicies,
  )
where

import qualified API.Types.UI.PolicyDocument as APIT
import qualified Dashboard.Common as Common
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Domain.Types.TransporterConfig as DTC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.PolicyAndComplianceDocument as CPCD
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD
import qualified Storage.Queries.PolicyAndComplianceDocumentExtra as QPCDX

acceptedPoliciesMap :: Maybe Common.AcceptedPolicies -> HM.HashMap Common.PolicyType Text
acceptedPoliciesMap = maybe HM.empty (\(Common.AcceptedPolicies m) -> m)

driverEntity :: Common.LegalEntityType
driverEntity = Common.DriverLegal

toRespDoc :: DPCD.PolicyAndComplianceDocument -> APIT.PolicyDocumentResp
toRespDoc d =
  APIT.PolicyDocumentResp
    { id = d.id,
      policyType = d.policyType,
      entityType = d.entityType,
      version = d.version,
      url = d.url,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      effectiveDate = d.effectiveDate,
      createdAt = d.createdAt
    }

toRespWithAcceptance :: HM.HashMap Common.PolicyType Text -> DPCD.PolicyAndComplianceDocument -> APIT.PolicyDocumentWithAcceptance
toRespWithAcceptance accepted d =
  APIT.PolicyDocumentWithAcceptance
    { id = d.id,
      policyType = d.policyType,
      entityType = d.entityType,
      version = d.version,
      url = d.url,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      effectiveDate = d.effectiveDate,
      createdAt = d.createdAt,
      accepted = HM.lookup d.policyType accepted == Just d.id.getId
    }

featureEnabled :: DTC.TransporterConfig -> Bool
featureEnabled tc = fromMaybe False tc.enableLegalComplianceDocuments

loadTransporterConfig :: Id DMOC.MerchantOperatingCity -> Flow DTC.TransporterConfig
loadTransporterConfig opCityId =
  getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = opCityId.getId}) Nothing
    >>= fromMaybeM (TransporterConfigNotFound opCityId.getId)

getPolicyLatest ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow APIT.PolicyLatestResp
getPolicyLatest (_, merchantId, opCityId) = do
  tc <- loadTransporterConfig opCityId
  if not (featureEnabled tc)
    then pure $ APIT.PolicyLatestResp {documents = []}
    else do
      docs <- CPCD.findAllLatestEnabledByMerchantAndEntity merchantId driverEntity
      pure $ APIT.PolicyLatestResp {documents = map toRespDoc docs}

getPolicyList ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow APIT.PolicyListResp
getPolicyList (mbPersonId, merchantId, opCityId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  tc <- loadTransporterConfig opCityId
  if not (featureEnabled tc)
    then pure $ APIT.PolicyListResp {groups = []}
    else do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let accepted = acceptedPoliciesMap person.acceptedPolicies
      latestPerType <- CPCD.findAllLatestEnabledByMerchantAndEntity merchantId driverEntity
      let types = L.nub (map (.policyType) latestPerType)
      groups <- forM types $ \pt -> do
        topN <- QPCDX.findTopNEnabledByTypeAndMerchantAndEntity merchantId driverEntity pt 2
        acceptedDoc <- case HM.lookup pt accepted of
          Nothing -> pure Nothing
          Just docIdText -> do
            mbDoc <- QPCD.findByPrimaryKey (Id docIdText)
            pure $ mbDoc >>= \d -> if d.merchantId == merchantId then Just d else Nothing
        let allDocs = maybe topN (\d -> if any ((== d.id) . (.id)) topN then topN else topN ++ [d]) acceptedDoc
        pure $ APIT.PolicyTypeGroup {policyType = pt, entries = map (toRespWithAcceptance accepted) allDocs}
      pure $ APIT.PolicyListResp {groups}

postPolicyAccept ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  APIT.PolicyAcceptReq ->
  Flow APISuccess
postPolicyAccept (mbPersonId, merchantId, opCityId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  tc <- loadTransporterConfig opCityId
  unless (featureEnabled tc) $
    throwError (InvalidRequest "Legal compliance documents feature is disabled")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  doc <-
    QPCD.findByPrimaryKey req.policyDocId
      >>= fromMaybeM (InvalidRequest $ "Policy document not found: " <> req.policyDocId.getId)
  unless (doc.merchantId == merchantId) $
    throwError (InvalidRequest "Policy document does not belong to caller's merchant")
  let accepted = acceptedPoliciesMap person.acceptedPolicies
      accepted' = HM.insert doc.policyType doc.id.getId accepted
  QPerson.updateAcceptedPolicies personId (Just (Common.AcceptedPolicies accepted'))
  pure Success

isGoOnlineBlockerActive :: DTC.TransporterConfig -> Bool
isGoOnlineBlockerActive tc =
  featureEnabled tc && fromMaybe False tc.enableGoOnlinePolicyBlocker

findBlockingUnacceptedPolicies :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DP.Person -> m [DPCD.PolicyAndComplianceDocument]
findBlockingUnacceptedPolicies person = do
  latest <- CPCD.findAllLatestEnabledByMerchantAndEntity person.merchantId driverEntity
  let accepted = acceptedPoliciesMap person.acceptedPolicies
  pure
    [ d
      | d <- latest,
        d.isMandatory,
        HM.lookup d.policyType accepted /= Just d.id.getId
    ]
