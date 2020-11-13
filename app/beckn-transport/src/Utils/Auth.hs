{-# LANGUAGE OverloadedLabels #-}

module Utils.Auth where

import App.Types
import Beckn.Types.App (OrganizationId (..))
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM401, throwError401)
import qualified Beckn.Utils.Registry as R
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.ByteString.Base64 as Base64
import EulerHS.Prelude
import qualified Storage.Queries.Organization as Org

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Organization
  verificationDescription =
    "Checks whether app/gateway is registered.\
    \If you don't have an API key, register the app/gateway."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction $ Org.findOrgByApiKey >=> fromMaybeM401 "INVALID_API_KEY"

data LookupRegistry = LookupRegistry

instance LookupMethod LookupRegistry where
  type LookupResult LookupRegistry = Organization
  lookupDescription =
    "Looks up the given key ID in the Beckn registry."

lookupRegistryAction :: LookupAction LookupRegistry AppEnv
lookupRegistryAction = LookupAction $ \keyId -> do
  cred <- R.lookup keyId >>= fromMaybeM401 "INVALID_KEY_ID"
  org <- Org.findOrganizationById $ OrganizationId $ cred ^. #_orgId
  pk <- case Base64.decode . encodeUtf8 $ cred ^. #_signPubKey of
    Left _ -> throwError401 "INVALID_PUBLIC_KEY"
    Right key -> return key
  return (org, pk)
