module API.Beckn.OnTrack (API, handler) where

import qualified Beckn.ACL.OnTrack as ACL
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = OnTrack.OnTrackAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onTrack

onTrack ::
  SignatureAuthResult ->
  OnTrack.OnTrackReq ->
  FlowHandler AckResponse
onTrack (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnTrackReq <- ACL.buildOnTrackReq req
  whenJust mbDOnTrackReq (DOnTrack.onTrack registryUrl)
  pure Ack
