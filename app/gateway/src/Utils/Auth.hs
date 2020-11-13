module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM401, throwError401)
import qualified Beckn.Utils.Registry as R
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.ByteString.Base64 as Base64
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
lookupRegistryAction = LookupAction $ \keyId -> do
  cred <- R.lookup keyId >>= fromMaybeM401 "INVALID_KEY_ID"
  org <- Org.findOrgById (cred ^. #_orgId) >>= fromMaybeM401 "INVALID_KEY_ID"
  pk <- case Base64.decode . encodeUtf8 $ cred ^. #_signPubKey of
    Left _ -> throwError401 "INVALID_PUBLIC_KEY"
    Right key -> return key
  return (org, pk)
