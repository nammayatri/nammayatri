module Utils.Auth where

import App.Types
import Beckn.Utils.Common (fromMaybeM400)
import Beckn.Utils.Servant.Auth
import EulerHS.Prelude

-- | TODO: Perform some API key verification.
type APIKeyAuth = TokenAuth' "X-API-TOKEN" VerifyAPIKey

data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = ()
  verificationDescription =
    "Checks whether gateway/provider is registered.\
    \If you don't have an API key, register the gateway/provider."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction $ \_ -> return ()
