module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as SOrganization
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
  ( LookupAction,
    LookupRegistry,
    lookupRegistryAction,
  )
import Storage.Queries.Organization (findOrgByShortId)
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

lookup :: LookupAction LookupRegistry AppEnv
lookup = lookupRegistryAction findOrgByShortId
