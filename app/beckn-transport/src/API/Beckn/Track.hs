module API.Beckn.Track (API, handler) where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Track as DTrack
import Domain.Types.Organization (Organization)
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import Servant hiding (throwError)
import Utils.Common

type API =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.TrackAPI

handler :: FlowServer API
handler = track

track ::
  Id Organization ->
  SignatureAuthResult ->
  API.TrackReq ->
  FlowHandler AckResponse
track transporterId (SignatureAuthResult signPayload subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "track API Flow" "Reached"
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dTrackReq <- ACL.buildTrackReq subscriber req
    let context = req.context
    dTrackRes <- DTrack.track transporterId dTrackReq
    ExternalAPI.withCallback dTrackRes.transporter Context.TRACK OnTrack.onTrackAPI context context.bap_uri $
      -- there should be DOnTrack.onTrack, but it is empty anyway
      pure $ ACL.mkOnTrackMessage dTrackRes
