module Utils.Auth where

import App.Types
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
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

lookup :: LookupAction LookupRegistry AppEnv
lookup = lookupRegistryAction Org.findOrganizationByShortId

getHttpManagerKey :: Text -> Flow String
getHttpManagerKey keyId = do
  pure $ signatureAuthManagerKey <> "-" <> T.unpack keyId
