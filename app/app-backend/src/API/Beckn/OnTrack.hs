module API.Beckn.OnTrack (API, handler) where

import App.Types
import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnTrack as ACL
import qualified Domain.Action.Beckn.OnTrack as DOnTrack

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
