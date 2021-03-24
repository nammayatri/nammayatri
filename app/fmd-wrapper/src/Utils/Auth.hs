module Utils.Auth where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM)
import Beckn.Utils.Servant.HeaderAuth
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
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
verifyApiKey = VerificationAction (Org.findOrgByApiKey >=> fromMaybeM OrgNotFound)

lookup :: HttpSig.LookupAction HttpSig.LookupRegistry AppEnv
lookup = HttpSig.lookupRegistryAction Org.findOrgByShortId
