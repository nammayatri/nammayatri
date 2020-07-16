module Utils.Auth where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fromMaybeM400)
import Beckn.Utils.Servant.Auth
import EulerHS.Prelude
import qualified Storage.Queries.App as BA
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
  app <- BA.lookupKey apiKey
  provider <- BP.lookupKey apiKey
  app <|> provider
    & fromMaybeM400 "INVALID_API_KEY" -- FIXME: Bad HTTP code
