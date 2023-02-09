module Tools.Auth where

import Kernel.InternalAPI.Auth.Client
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (Context)

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- TODO: make common Person across all our BAPs
data Person

type PersonId = Id Person

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = PersonId
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPersonAction ::
  forall m r.
  ( CoreMetrics m,
    HasFlowEnv m r '["authServiceUrl" ::: BaseUrl]
  ) =>
  VerificationAction VerifyToken m
verifyPersonAction = VerificationAction (fmap Id . auth)
