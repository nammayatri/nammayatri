{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Utils.Auth where

import App.Types
import Beckn.Types.App (ShortOrganizationId (..))
import qualified Beckn.Types.Storage.Organization as SOrganization
import Beckn.Utils.Common
import qualified Beckn.Utils.Registry as R
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Organization as QOrganization

type VerificationAPIKey = APIKeyAuth VerifyAPIKey

data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = SOrganization.Organization
  verificationDescription =
    "Checks whether gateway/provider is registered.\
    \If you don't have an API key, register the gateway/provider."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction QOrganization.verifyApiKey

data LookupRegistry = LookupRegistry

instance LookupMethod LookupRegistry where
  type LookupResult LookupRegistry = SOrganization.Organization
  lookupDescription =
    "Looks up the given key ID in the Beckn registry."

lookupRegistryAction :: LookupAction LookupRegistry AppEnv
lookupRegistryAction = LookupAction $ \signaturePayload -> do
  selfUrl <- ask >>= fromMaybeM500 "NO_SELF_URL" . bapNwAddress
  L.logDebug @Text "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  let keyId = signaturePayload ^. #params . #keyId . #uniqueKeyId
  mCred <- R.lookupKey keyId
  cred <- case mCred of
    Just c -> return c
    Nothing -> do
      L.logError @Text "SignatureAuth" $ "Could not look up keyId: " <> keyId
      throwError401 "INVALID_KEY_ID"
  org <-
    QOrganization.findOrgByShortId (ShortOrganizationId $ cred ^. #_orgId)
      >>= maybe (throwError401 "ORG_NOT_FOUND") pure
  pk <- case R.decodeKey $ cred ^. #_signPubKey of
    Nothing -> do
      L.logError @Text "SignatureAuth" $ "Invalid public key: " <> show (cred ^. #_signPubKey)
      throwError401 "INVALID_PUBLIC_KEY"
    Just key -> return key
  return (org, pk, selfUrl)
