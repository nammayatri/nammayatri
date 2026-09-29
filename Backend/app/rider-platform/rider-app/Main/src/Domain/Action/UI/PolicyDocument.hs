module Domain.Action.UI.PolicyDocument
  ( getPolicyLatest,
    getPolicyList,
    postPolicyAccept,
    parseAcceptedPolicies,
    serializeAcceptedPolicies,
    computePendingLegalPolicies,
    PendingLegalPolicy (..),
  )
where

import qualified API.Types.UI.PolicyDocument as APIT
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.OpenApi (ToSchema)
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Domain.Types.RiderConfig as DRC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.PolicyAndComplianceDocument as CPCD
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD
import qualified Storage.Queries.PolicyAndComplianceDocumentExtra as QPCDX
import Tools.Error

parseAcceptedPolicies :: MonadFlow m => Maybe Text -> m (HM.HashMap Text Text)
parseAcceptedPolicies Nothing = pure HM.empty
parseAcceptedPolicies (Just raw) =
  case A.decodeStrict (encodeUtf8 raw) of
    Just m -> pure m
    Nothing -> do
      logError $ "Failed to parse Person.acceptedPolicies JSON: " <> raw
      pure HM.empty

serializeAcceptedPolicies :: HM.HashMap Text Text -> Text
serializeAcceptedPolicies = TE.decodeUtf8 . LBS.toStrict . A.encode

toRespDoc :: DPCD.PolicyAndComplianceDocument -> APIT.PolicyDocumentResp
toRespDoc d =
  APIT.PolicyDocumentResp
    { id = d.id,
      policyType = d.policyType,
      version = d.version,
      url = d.url,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      createdAt = d.createdAt
    }

toRespWithAcceptance :: HM.HashMap Text Text -> DPCD.PolicyAndComplianceDocument -> APIT.PolicyDocumentWithAcceptance
toRespWithAcceptance accepted d =
  APIT.PolicyDocumentWithAcceptance
    { id = d.id,
      policyType = d.policyType,
      version = d.version,
      url = d.url,
      isMandatory = d.isMandatory,
      enabled = d.enabled,
      metadata = d.metadata,
      createdAt = d.createdAt,
      accepted = HM.lookup d.policyType accepted == Just d.id.getId
    }

featureEnabled :: DRC.RiderConfig -> Bool
featureEnabled riderCfg = fromMaybe False riderCfg.enableLegalComplianceDocuments

loadCfg :: Id DP.Person -> Flow (DP.Person, DRC.RiderConfig)
loadCfg personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderCfg <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  pure (person, riderCfg)

getPolicyLatest ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Flow APIT.PolicyLatestResp
getPolicyLatest (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  (_, riderCfg) <- loadCfg personId
  if not (featureEnabled riderCfg)
    then pure $ APIT.PolicyLatestResp {documents = []}
    else do
      docs <- CPCD.findAllLatestEnabledByMerchant merchantId
      pure $ APIT.PolicyLatestResp {documents = map toRespDoc docs}

getPolicyList ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Flow APIT.PolicyListResp
getPolicyList (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  (person, riderCfg) <- loadCfg personId
  if not (featureEnabled riderCfg)
    then pure $ APIT.PolicyListResp {groups = []}
    else do
      accepted <- parseAcceptedPolicies person.acceptedPolicies
      latestPerType <- CPCD.findAllLatestEnabledByMerchant merchantId
      let types = L.nub (map (.policyType) latestPerType)
      groups <- forM types $ \pt -> do
        topN <- QPCDX.findTopNEnabledByTypeAndMerchant merchantId pt 2
        acceptedDoc <- case HM.lookup pt accepted of
          Nothing -> pure Nothing
          Just docIdText -> do
            mbDoc <- QPCD.findByPrimaryKey (Id docIdText)
            pure $ mbDoc >>= \d -> if d.merchantId == merchantId then Just d else Nothing
        let allDocs = maybe topN (\d -> if any ((== d.id) . (.id)) topN then topN else topN ++ [d]) acceptedDoc
        pure $ APIT.PolicyTypeGroup {policyType = pt, entries = map (toRespWithAcceptance accepted) allDocs}
      pure $ APIT.PolicyListResp {groups}

postPolicyAccept ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  APIT.PolicyAcceptReq ->
  Flow APISuccess
postPolicyAccept (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  doc <-
    QPCD.findByPrimaryKey req.policyDocId
      >>= fromMaybeM (InvalidRequest $ "Policy document not found: " <> req.policyDocId.getId)
  unless (doc.merchantId == merchantId) $
    throwError (InvalidRequest "Policy document does not belong to caller's merchant")
  accepted <- parseAcceptedPolicies person.acceptedPolicies
  let accepted' = HM.insert doc.policyType doc.id.getId accepted
  QPerson.updateAcceptedPolicies personId (Just (serializeAcceptedPolicies accepted'))
  pure Success

data PendingLegalPolicy = PendingLegalPolicy
  { policyType :: Text,
    docId :: Id DPCD.PolicyAndComplianceDocument,
    version :: Text,
    url :: Text,
    isMandatory :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

computePendingLegalPolicies :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DP.Person -> DRC.RiderConfig -> m (Maybe [PendingLegalPolicy])
computePendingLegalPolicies person riderCfg
  | not (featureEnabled riderCfg) = pure Nothing
  | otherwise = do
    latest <- CPCD.findAllLatestEnabledByMerchant person.merchantId
    if null latest
      then pure Nothing
      else do
        accepted <- parseAcceptedPolicies person.acceptedPolicies
        let pending =
              [ PendingLegalPolicy
                  { policyType = d.policyType,
                    docId = d.id,
                    version = d.version,
                    url = d.url,
                    isMandatory = d.isMandatory
                  }
                | d <- latest,
                  HM.lookup d.policyType accepted /= Just d.id.getId
              ]
        pure $ if null pending then Nothing else Just pending
