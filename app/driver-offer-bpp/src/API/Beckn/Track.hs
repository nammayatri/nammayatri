module API.Beckn.Track (API, handler) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import qualified Domain.Action.Beckn.Track as DTrack
import qualified Domain.Types.Organization as Org
import Environment
import ExternalAPI.Flow
import Servant
import Utils.Common

type API =
  Capture "orgId" (Id Org.Organization)
    :> SignatureAuth "Authorization"
    :> Track.TrackAPI

handler :: FlowServer API
handler = track

track ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Track.TrackReq ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "track API Flow" "Reached"
    dTrackReq <- ACL.buildTrackReq subscriber req
    let context = req.context
    dTrackRes <- DTrack.track transporterId dTrackReq
    withCallback dTrackRes.transporter Context.TRACK OnTrack.onTrackAPI context context.bap_uri $
      -- there should be DOnTrack.onTrack, but it is empty anyway
      pure $ ACL.mkOnTrackMessage dTrackRes
