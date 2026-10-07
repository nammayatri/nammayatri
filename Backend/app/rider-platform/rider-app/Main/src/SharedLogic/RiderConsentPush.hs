module SharedLogic.RiderConsentPush
  ( pushConsent,
    syncConsentIfNeeded,
    mkConsentItem,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Id (cast)
import Kernel.Utils.Common
import qualified Safety.Storage.BeamFlow as SafetyBeam
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import Tools.Metrics (CoreMetrics)

mkConsentItem :: (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) => DP.Person -> Bool -> m (Maybe CallBPPInternal.RiderConsentItem)
mkConsentItem person consent = do
  mbNumber <- mapM decrypt person.mobileNumber
  moc <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  pure $ do
    number <- mbNumber
    countryCode <- person.mobileCountryCode
    pure
      CallBPPInternal.RiderConsentItem
        { customerMobileNumber = fromMaybe number (T.stripPrefix "+91" number),
          customerMobileCountryCode = countryCode,
          city = moc.city,
          consentToShareMobileNumber = consent
        }

pushConsent ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  DP.Person ->
  Bool ->
  m ()
pushConsent person consent = do
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  mkConsentItem person consent >>= \case
    Nothing -> logWarning $ "Skipping consent push, rider has no mobile number: " <> person.id.getId
    Just item ->
      withTryCatch "pushConsent" (CallBPPInternal.setRiderConsent merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId (CallBPPInternal.SetRiderConsentReq merchant.bapId [item])) >>= \case
        Right res | null res.failedIndices -> pure ()
        Right _ -> failPush "BPP rejected the update"
        Left err -> failPush (show err)
  where
    failPush reason = do
      logError $ "Consent push to BPP failed for " <> person.id.getId <> ": " <> reason
      throwError (InternalError "Unable to update number sharing preference, please try again")

-- One-time catch-up: pushes consent chosen before the BPP push existed (or while
-- push_consent_to_bpp was off) when the rider next opens the app. Temporary;
-- remove with consent_synced_at once the consent tag and push flag are retired
-- (target: Jan 2027).
syncConsentIfNeeded ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    SafetyBeam.BeamFlow m r
  ) =>
  DP.Person ->
  m ()
syncConsentIfNeeded person =
  QSafetySettings.findByPersonId (cast person.id) >>= \case
    Just settings | isNothing settings.consentSyncedAt ->
      whenJust settings.consentToShareMobileNumber $ \consent -> do
        pushConsent person consent
        now <- getCurrentTime
        QSafetySettings.updateConsentSyncedAt (Just now) (cast person.id)
    _ -> pure ()
