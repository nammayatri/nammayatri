{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import Control.Lens.Operators ((?~))
import EulerHS.Prelude hiding (drop)
import qualified EulerHS.Types as ET
import qualified ExternalAPI.Dunzo.Flow as API
import ExternalAPI.Dunzo.Types
import Product.Dunzo.Transform
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.SearchRequest as Storage
import qualified Types.Beckn.API.Cancel as CancelAPI
import qualified Types.Beckn.API.Confirm as ConfirmAPI
import qualified Types.Beckn.API.Search as SearchAPI
import qualified Types.Beckn.API.Status as StatusAPI
import qualified Types.Beckn.API.Track as TrackAPI
import Types.Beckn.Context (Action (..))
import Types.Beckn.DecimalValue (convertDecimalValueToAmount)
import Types.Beckn.Order
import Types.Common
import Types.Error
import qualified Types.Storage.Organization as Org
import Types.Storage.SearchRequest as SearchRequest
import Types.Wrapper
import Utils.Common

search ::
  Org.Organization ->
  BaseUrl ->
  BecknReq SearchAPI.SearchIntent ->
  Flow AckResponse
search org cbUrl req = do
  config@DunzoConfig {..} <- asks (.dzConfig)
  quoteReq <- mkQuoteReqFromSearch req
  dzBACreds <- getDzBAPCreds org
  withCallback' withRetry SEARCH SearchAPI.onSearchAPI req.context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

confirm ::
  Org.Organization ->
  BecknReq OrderObject ->
  Flow AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- asks (.dzConfig)
  let reqOrder = req.message.order
  whenJust reqOrder.id \_ -> throwError $ InvalidRequest "Order id must not be presented."
  whenNothing_
    (reqOrder ^. #payment . #params . _Just . #transaction_id)
    $ throwError (ErrorCodeWithMessage "TXN_ID_NOT_PRESENT" CORE001)
  dzBACreds <- getDzBAPCreds org
  orderId <- generateGUID
  currTime <- getCurrentTime
  taskReq <- mkCreateTaskReq orderId reqOrder
  withCallback CONFIRM ConfirmAPI.onConfirmAPI req.context req.context.bap_uri do
    taskStatus <- createTaskAPI dzBACreds dconf taskReq
    let uOrder = updateOrder (Just orderId) currTime reqOrder taskStatus
    checkAndLogPriceDiff reqOrder uOrder
    createSearchRequest orderId uOrder taskStatus currTime
    return $ OrderObject uOrder
  where
    createSearchRequest orderId order taskStatus now = do
      let searchRequestId = Id req.context.transaction_id
      let searchRequest =
            SearchRequest
              { id = searchRequestId,
                name = Nothing,
                description = Nothing,
                shortId = ShortId orderId,
                industry = GROCERY,
                _type = RIDEORDER,
                exchangeType = ORDER,
                status = CONFIRMED,
                startTime = now,
                endTime = Nothing,
                validTill = now,
                provider = Just "Dunzo",
                providerType = Nothing,
                requestor = Just org.id.getId,
                requestorType = Nothing,
                udf1 = Just $ encodeToText order,
                udf2 = Just $ encodeToText taskStatus,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      Storage.create searchRequest

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder.id
      let initPrice = convertDecimalValueToAmount =<< (initOrder.payment ^? #params . _Just . #amount . _Just)
      let confirmPrice = convertDecimalValueToAmount =<< (confirmOrder.payment ^? #params . _Just . #amount . _Just)
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

track ::
  Org.Organization ->
  BecknReq TrackAPI.TrackInfo ->
  Flow AckResponse
track org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  searchRequest <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  dzBACreds <- getDzBAPCreds org
  withCallback TRACK TrackAPI.onTrackAPI req.context req.context.bap_uri $
    getStatus dzBACreds conf (TaskId taskId) <&> mkOnTrackMessage . (.tracking_url)

status ::
  Org.Organization ->
  BecknReq StatusAPI.OrderId ->
  Flow AckResponse
status org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  searchRequest <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <-
    searchRequest.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallback STATUS StatusAPI.onStatusAPI req.context req.context.bap_uri do
    taskStatus <- getStatus dzBACreds conf (TaskId taskId)
    onStatusMessage <- mkOnStatusMessage order taskStatus
    let updatedOrder = onStatusMessage.order
    updateSearchRequest (searchRequest.id) updatedOrder taskStatus searchRequest
    return onStatusMessage
  where
    updateSearchRequest searchRequestId order taskStatus searchRequest = do
      let updatedSearchRequest = searchRequest {udf1 = Just $ encodeToText order, udf2 = Just $ encodeToText taskStatus}
      Storage.update searchRequestId updatedSearchRequest

cancel ::
  Org.Organization ->
  BecknReq CancelAPI.CancellationInfo ->
  Flow AckResponse
cancel org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  searchRequest <- Storage.findById (Id req.context.transaction_id) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <- searchRequest.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallback CANCEL CancelAPI.onCancelAPI req.context req.context.bap_uri do
    callCancelAPI dzBACreds conf (TaskId taskId)
    let updatedOrder = cancelOrder order
    updateSearchRequest searchRequest.id updatedOrder searchRequest
    return $ OrderObject updatedOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateSearchRequest :: DBFlow m r => Id SearchRequest -> Order -> SearchRequest -> m ()
    updateSearchRequest searchRequestId order searchRequest = do
      let updatedSearchRequest = searchRequest {udf1 = Just $ encodeToText order}
      Storage.update searchRequestId updatedSearchRequest

-- Helpers
getQuote ::
  DzBAConfig ->
  DunzoConfig ->
  QuoteReq ->
  Flow QuoteRes
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus ::
  DzBAConfig ->
  DunzoConfig ->
  TaskId ->
  Flow TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken ::
  DzBAConfig ->
  DunzoConfig ->
  Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

withCallback ::
  WithBecknCallbackMig api callback_success Flow
withCallback = withCallback' identity

withCallback' ::
  (Flow () -> Flow ()) ->
  WithBecknCallbackMig api callback_success Flow
withCallback' doWithCallback action api context cbUrl f = do
  bppUri <- asks (.nwAddress)
  selfId <- asks (.selfId)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ selfId
  withBecknCallbackMig doWithCallback authKey action api context' cbUrl f

authKey :: Maybe ET.ManagerSelector
authKey = Just HttpSig.signatureAuthManagerKey
