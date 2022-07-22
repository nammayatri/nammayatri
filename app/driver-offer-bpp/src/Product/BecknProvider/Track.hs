module Product.BecknProvider.Track where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import qualified Domain.Action.Beckn.Track as DTrack
import qualified Domain.Types.Organization as Org
import Environment
import ExternalAPI.Flow
import Utils.Common

track ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Track.TrackReq ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "track API Flow" "Reached"
    dTrackReq <- ACL.buildTrackReq subscriber req
    let context = req.context
    dTrackRes <- DTrack.track transporterId dTrackReq
    withCallback dTrackRes.transporter Context.TRACK OnTrack.onTrackAPI context context.bap_uri $
      -- there should be DOnTrack.onTrack, but it is empty anyway
      pure $ ACL.mkOnTrackMessage dTrackRes
