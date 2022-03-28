module Tools.Auth where

import Beckn.InternalAPI.Auth.Client
import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Domain.Search (Person)
import Servant hiding (Context)
import Tools.Metrics (CoreMetrics)

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

type PersonId = Id Person

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = PersonId
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPersonAction ::
  forall m r.
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasField "authServiceUrl" r BaseUrl
  ) =>
  VerificationAction VerifyToken m
verifyPersonAction = VerificationAction (fmap Id . auth)
