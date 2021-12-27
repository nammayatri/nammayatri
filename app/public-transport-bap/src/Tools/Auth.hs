module Tools.Auth where

import Beckn.InternalAPI.Auth.Client
import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
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
  forall m r c.
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasInConfig r c "authServiceUrl" BaseUrl
  ) =>
  VerificationAction VerifyToken m
verifyPersonAction = VerificationAction (fmap Id . auth)
