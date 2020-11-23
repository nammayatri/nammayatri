{-# LANGUAGE TypeApplications #-}

module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM401, throwError401)
import qualified Beckn.Utils.Registry as R
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.App as BA
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Provider as BP

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Org.Organization
  verificationDescription =
    "Checks whether app/provider is registered.\
    \If you don't have an API key, register with the gateway."

verifyAPIKeyAction :: VerificationAction VerifyAPIKey AppEnv
verifyAPIKeyAction = VerificationAction $ \apiKey -> do
  app <- BA.lookupKey Org.APP apiKey
  provider <- BP.lookupKey Org.PROVIDER apiKey
  app <|> provider
    & fromMaybeM401 "INVALID_API_KEY"

data LookupRegistry = LookupRegistry

instance LookupMethod LookupRegistry where
  type LookupResult LookupRegistry = Org.Organization
  lookupDescription =
    "Looks up the given key ID in the Beckn registry."

lookupRegistryAction :: LookupAction LookupRegistry AppEnv
lookupRegistryAction = LookupAction $ \signaturePayload -> do
  L.logDebug @Text "SignatureAuth" $ "Got Signature: " <> show (HttpSig.encode signaturePayload)
  let keyId = signaturePayload ^. #params . #keyId . #uniqueKeyId
  mCred <- R.lookupKey keyId
  cred <- case mCred of
    Just c -> return c
    Nothing -> do
      L.logError @Text "SignatureAuth" $ "Could not look up keyId: " <> keyId
      throwError401 "INVALID_KEY_ID"
  mOrg <- Org.findOrgByShortId (cred ^. #_orgId)
  org <- case mOrg of
    Just o -> return o
    Nothing -> do
      L.logError @Text "SignatureAuth" $ "Could not look up orgId: " <> cred ^. #_orgId
      throwError401 "INVALID_KEY_ID"
  pk <- case R.decodeKey $ cred ^. #_signPubKey of
    Nothing -> do
      L.logError @Text "SignatureAuth" $ "Invalid public key: " <> show (cred ^. #_signPubKey)
      throwError401 "INVALID_PUBLIC_KEY"
    Just key -> return key
  return (org, pk)
