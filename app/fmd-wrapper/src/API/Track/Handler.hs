module API.Track.Handler where

import API.Common
import qualified API.Track.Types as Track
import App.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.Beckn.API.OnTrack as OnTrack
import Types.Beckn.Context
import Types.Error
import qualified Types.Storage.Organization as SOrg
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> BecknReq Track.TrackInfo -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext TRACK $ req.context
    validateBapUrl subscriber $ req.context
    track bapOrg req

track ::
  SOrg.Organization ->
  BecknReq Track.TrackInfo ->
  Flow AckResponse
track org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  searchRequest <- QSearchRequest.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: Dz.TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  dzBACreds <- getDzBAPCreds org
  withCallback TRACK OnTrack.onTrackAPI req.context req.context.bap_uri $
    getStatus dzBACreds conf (Dz.TaskId taskId) <&> mkOnTrackMessage . (.tracking_url)

mkOnTrackMessage :: Maybe BaseUrl -> OnTrack.OnTrackInfo
mkOnTrackMessage mbTrackingUrl = OnTrack.OnTrackInfo tracking
  where
    tracking =
      OnTrack.Tracking
        { url = mbTrackingUrl,
          status = Nothing
        }
