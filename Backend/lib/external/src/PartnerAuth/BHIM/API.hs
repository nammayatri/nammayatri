module PartnerAuth.BHIM.API where

import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import PartnerAuth.BHIM.Types
import Servant hiding (throwError)

-- Common envelope: POST {BHIM_BASE}/<path> with header partnerID and body
-- { encRequest }, response { encResponseBody }.
type VerifyTokenAPI =
  "pwa" :> "api" :> "v2" :> "verify-token"
    :> Header "partnerID" Text
    :> ReqBody '[JSON] EncEnvelope
    :> Post '[JSON] EncEnvelopeRes

type UserDetailsAPI =
  "pwa" :> "api" :> "v2" :> "user-details"
    :> Header "partnerID" Text
    :> ReqBody '[JSON] EncEnvelope
    :> Post '[JSON] EncEnvelopeRes

verifyTokenClient :: Maybe Text -> EncEnvelope -> EulerClient EncEnvelopeRes
verifyTokenClient = client (Proxy :: Proxy VerifyTokenAPI)

userDetailsClient :: Maybe Text -> EncEnvelope -> EulerClient EncEnvelopeRes
userDetailsClient = client (Proxy :: Proxy UserDetailsAPI)

callVerifyToken :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Maybe Text -> EncEnvelope -> m EncEnvelopeRes
callVerifyToken url partnerId req =
  callAPI url (verifyTokenClient partnerId req) "bhimVerifyToken" (Proxy :: Proxy VerifyTokenAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call BHIM verify-token API: " <> show err)

callUserDetails :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Maybe Text -> EncEnvelope -> m EncEnvelopeRes
callUserDetails url partnerId req =
  callAPI url (userDetailsClient partnerId req) "bhimUserDetails" (Proxy :: Proxy UserDetailsAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call BHIM user-details API: " <> show err)
