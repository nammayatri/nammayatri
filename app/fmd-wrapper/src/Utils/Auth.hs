module Utils.Auth where

import App.Types
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM400)
import Beckn.Utils.Servant.HeaderAuth
import Control.Monad ()
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
verifyApiKey = VerificationAction (Org.findOrgByApiKey >=> fromMaybeM400 "INVALID_API_KEY")
