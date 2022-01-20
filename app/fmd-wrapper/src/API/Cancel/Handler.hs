module API.Cancel.Handler where

import qualified API.Cancel.Types as Cancel
import API.Common
import App.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.Beckn.API.OnCancel as OnCancel
import qualified Types.Beckn.API.OnConfirm as OnConfirm
import Types.Beckn.Context
import Types.Error
import qualified Types.Storage.Organization as SOrg
import qualified Types.Storage.SearchRequest as SSearchRequest
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> BecknReq Cancel.CancellationInfo -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CANCEL $ req.context
    validateBapUrl subscriber $ req.context
    cancel bapOrg req

cancel ::
  SOrg.Organization ->
  BecknReq Cancel.CancellationInfo ->
  Flow AckResponse
cancel org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  searchRequest <- QSearchRequest.findById (Id req.context.transaction_id) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: Dz.TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <- searchRequest.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallback CANCEL OnCancel.onCancelAPI req.context req.context.bap_uri do
    callCancelAPI dzBACreds conf (Dz.TaskId taskId)
    let updatedOrder = cancelOrder order
    let onCancelOrder = mkOnCancelOrder order
    updateSearchRequest searchRequest.id updatedOrder searchRequest
    return $ OnCancel.OrderObject onCancelOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      DzAPI.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateSearchRequest :: DBFlow m r => Id SSearchRequest.SearchRequest -> OnConfirm.Order -> SSearchRequest.SearchRequest -> m ()
    updateSearchRequest searchRequestId order searchRequest = do
      let updatedSearchRequest = searchRequest {SSearchRequest.udf1 = Just $ encodeToText order}
      QSearchRequest.update searchRequestId updatedSearchRequest

cancelOrder :: OnConfirm.Order -> OnConfirm.Order
cancelOrder order = order & #state .~ "CANCELLED"

mkOnCancelOrder :: OnConfirm.Order -> OnCancel.Order
mkOnCancelOrder OnConfirm.Order {..} = do
  OnCancel.Order
    { state = "CANCELLED",
      ..
    }
