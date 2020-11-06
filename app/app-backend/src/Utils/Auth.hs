module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as SOrganization
import Beckn.Utils.Servant.HeaderAuth
import qualified Storage.Queries.Organization as QOrganization

-- | TODO: Perform some API key verification.
type VerificationAPIKey = APIKeyAuth VerifyAPIKey

data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = SOrganization.Organization
  verificationDescription =
    "Checks whether gateway/provider is registered.\
    \If you don't have an API key, register the gateway/provider."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction QOrganization.verifyApiKey
