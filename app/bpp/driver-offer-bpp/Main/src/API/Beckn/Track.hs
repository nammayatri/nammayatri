module API.Beckn.Track (API, handler) where

import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import Core.Beckn (withCallback')
import qualified Domain.Action.Beckn.Track as DTrack
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Track.TrackAPI

handler :: FlowServer API
handler = track

track ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Track.TrackReq ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "track API Flow" "Reached"
    dTrackReq <- ACL.buildTrackReq subscriber req
    let context = req.context
    dTrackRes <- DTrack.track transporterId dTrackReq
    withCallback' withShortRetry dTrackRes.transporter Context.TRACK OnTrack.onTrackAPI context context.bap_uri $
      -- there should be DOnTrack.onTrack, but it is empty anyway
      pure $ ACL.mkOnTrackMessage dTrackRes
