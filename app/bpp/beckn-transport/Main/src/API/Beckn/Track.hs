module API.Beckn.Track (API, handler) where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import Core.Beckn (withCallback')
import qualified Domain.Action.Beckn.Track as DTrack
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude
import Servant hiding (throwError)

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.TrackAPI

handler :: FlowServer API
handler = track

track ::
  Id Merchant ->
  SignatureAuthResult ->
  API.TrackReq ->
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
