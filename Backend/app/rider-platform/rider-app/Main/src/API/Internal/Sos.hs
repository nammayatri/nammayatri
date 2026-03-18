module API.Internal.Sos
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Sos as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Tools.Error

type API =
  "sos"
    :> "erss-reauth"
    :> Header "api-key" Text
    :> ReqBody '[JSON] Domain.ErssReauthReq
    :> Post '[JSON] Domain.ErssReauthResp

handler :: FlowServer API
handler =
  sosErssReauth

sosErssReauth :: Maybe Text -> Domain.ErssReauthReq -> FlowHandler Domain.ErssReauthResp
sosErssReauth apiKey req = withFlowHandlerAPI $ do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $
    throwError $ AuthBlocked "Invalid API key"
  Domain.postSosErssReauth req
