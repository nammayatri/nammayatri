{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.DecimalValue as M.DecimalValue
import qualified Beckn.Types.Core.Migration.Order as M.Order
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Callback (WithBecknCallback, WithBecknCallbackMig, withBecknCallback, withBecknCallbackMig)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
import qualified ExternalAPI.Dunzo.Flow as API
import ExternalAPI.Dunzo.Types
import Product.Dunzo.Transform
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import qualified Types.Beckn.API.Cancel as API
import qualified Types.Beckn.API.Confirm as ConfirmAPI
import qualified Types.Beckn.API.Init as API
import qualified Types.Beckn.API.Search as SearchAPI
import qualified Types.Beckn.API.Select as API
import qualified Types.Beckn.API.Status as API
import qualified Types.Beckn.API.Track as API
import qualified Types.Beckn.API.Types as API
import qualified Types.Beckn.API.Update as API
import qualified Types.Beckn.FmdItem as Item
import Types.Beckn.FmdOrder
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common

search :: Org.Organization -> API.BecknReq SearchAPI.SearchIntent -> Flow AckResponse
search org req = do
  config@DunzoConfig {..} <- dzConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUriMig (req.context) dzBPNwAddress
  bap <- Org.findByBapUrl context.bap_uri >>= fromMaybeM OrgDoesNotExist
  dzBACreds <- getDzBAPCreds bap
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withCallbackMig "search" SearchAPI.onSearchAPI context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

select :: Org.Organization -> API.SelectReq -> Flow API.SelectRes
select org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req.context) dzBPNwAddress
  validateOrderRequest $ req.message.order
  validateReturn $ req.message.order
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  dzBACreds <- getDzBAPCreds org
  withCallback "select" API.onSelectAPI context cbUrl $ do
    quoteRes <-
      mkQuoteReqFromSelect req
        >>= getQuote dzBACreds conf
    let reqOrder = req.message.order
    onSelectMessage <- mkOnSelectOrder reqOrder dzQuotationTTLinMin quoteRes
    let order = onSelectMessage.order
    -- onSelectMessage has quotation
    let quote = fromJust $ onSelectMessage.order.quotation
    let quoteId = quote.id
    let orderDetails = OrderDetails order quote
    Storage.storeQuote quoteId orderDetails
    return onSelectMessage
  where
    validateOrderRequest order = do
      let tasks = order.tasks
      when (length tasks /= 1) $ throwError (InvalidRequest "Currently processing only one task per order.")
      let task = head tasks
      let package = task.package
      let pickup = task.pickup
      let drop = task.drop
      when (isJust $ pickup.time) $ throwError $ InvalidRequest "Scheduled pickup not supported."
      when (isJust $ drop.time) $ throwError $ InvalidRequest "Scheduled drop not supported."
      void $ case readMaybe . T.unpack =<< (package.package_category_id) of
        Nothing -> throwError $ InvalidRequest "Invalid package category id."
        -- Category id is the index value of dzPackageContentList
        Just cid ->
          unless (cid > 0 && cid <= length dzPackageContentList) $
            throwError $ InvalidRequest "Invalid package category id."

init :: Org.Organization -> API.InitReq -> Flow API.InitRes
init org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  quote <- req.message.order.quotation & fromMaybeErr "INVALID_QUOTATION" (Just CORE003)
  let quoteId = quote.id
  payeeDetails <- payee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybeErr "INVALID_QUOTATION_ID" (Just CORE003)
  let order = orderDetails.order
  validateReturn order
  dzBACreds <- getDzBAPCreds org
  withCallback "init" API.onInitAPI context cbUrl $ do
    -- quoteId will be used as orderId
    onInitMessage <-
      mkQuoteReqFromSelect (API.SelectReq context (API.SelectOrder (orderDetails.order)))
        >>= getQuote dzBACreds conf
        >>= mkOnInitMessage
          quoteId
          dzQuotationTTLinMin
          (orderDetails.order)
          payeeDetails
          req
    createCaseIfNotPresent (getId $ org.id) (onInitMessage.order) (orderDetails.quote)
    return onInitMessage
  where
    createCaseIfNotPresent orgId order quote = do
      now <- getCurrentTime
      let caseId = Id $ fromJust $ order.id
      let case_ =
            Case
              { id = caseId,
                name = Nothing,
                description = Nothing,
                shortId = "", -- FIX this
                industry = GROCERY,
                _type = RIDEORDER,
                exchangeType = ORDER,
                status = NEW,
                startTime = now,
                endTime = Nothing,
                validTill = now,
                provider = Just "Dunzo",
                providerType = Nothing,
                requestor = Just orgId,
                requestorType = Nothing,
                parentCaseId = Nothing,
                fromLocationId = "",
                toLocationId = "",
                udf1 = Just $ encodeToText (OrderDetails order quote),
                udf2 = Nothing,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      mcase <- Storage.findById caseId
      case mcase of
        Nothing -> Storage.create case_
        Just _ -> pass

confirm :: Org.Organization -> API.BecknReq API.OrderObject -> Flow AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req.message.order
  orderId <- fromMaybeErr "INVALID_ORDER_ID" (Just CORE003) $ reqOrder.id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  (orderDetails :: OrderDetailsMig) <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let order = orderDetails.order
  validateDelayFromInit dzQuotationTTLinMin case_
  verifyPayment reqOrder order
  -- validateReturn order FIXME
  txnId <-
    reqOrder ^? #payment . #params . _Just . #transaction_id
      & fromMaybeErr "TXN_ID_NOT_PRESENT" Nothing
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #payment . #params . _Just . #transaction_id) .~ txnId)
  dzBACreds <- getDzBAPCreds org
  withCallbackMig "confirm" ConfirmAPI.onConfirmAPI context cbUrl $ do
    taskStatus <- createTaskAPI dzBACreds dconf =<< mkCreateTaskReq order
    currTime <- getCurrentTime
    let uOrder = updateOrderMig currTime (updatedOrderDetailsWTxn.order) taskStatus
    checkAndLogPriceDiff (updatedOrderDetailsWTxn.order) uOrder
    updateCase case_ (updatedOrderDetailsWTxn & #order .~ uOrder) taskStatus
    return $ API.OrderObject uOrder
  where
    verifyPayment :: M.Order.Order -> M.Order.Order -> Flow ()
    verifyPayment reqOrder order = do
      confirmAmount <-
        reqOrder ^? #payment . #params . _Just . #amount
          & fromMaybeErr "INVALID_PAYMENT_AMOUNT" (Just CORE003)
      orderAmount <-
        order ^? #payment . #params . _Just . #amount
          & fromMaybeErr "ORDER_AMOUNT_NOT_FOUND" (Just CORE003)
      if confirmAmount == orderAmount
        then pass
        else throwError (InvalidRequest "Invalid order amount.")

    updateCase case_ orderDetails taskStatus = do
      let caseId = case_.id
      let taskId = taskStatus.task_id
      let updatedCase =
            case_
              { shortId = ShortId $ getTaskId taskId,
                udf1 = Just $ encodeToText orderDetails,
                udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder.id
      let initPrice = M.DecimalValue.convertDecimalValueToAmount =<< (initOrder.payment ^? #params . _Just . #amount . _Just)
      let confirmPrice = M.DecimalValue.convertDecimalValueToAmount =<< (confirmOrder.payment ^? #params . _Just . #amount . _Just)
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

    validateDelayFromInit dzQuotationTTLinMin case_ = do
      now <- getCurrentTime
      let orderCreatedAt = case_.createdAt
      let thresholdTime = addUTCTime (fromInteger (dzQuotationTTLinMin * 60)) orderCreatedAt
      when (thresholdTime < now) $
        throwError (InvalidRequest "Took too long to confirm.")

track :: Org.Organization -> API.TrackReq -> Flow API.TrackRes
track org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let orderId = req.message.order_id
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  withCallback "track" API.onTrackAPI context cbUrl $ do
    let taskId = getShortId case_.shortId
    dzBACreds <- getDzBAPCreds org
    statusRes <- getStatus dzBACreds conf (TaskId taskId)
    return $ mkOnTrackMessage orderId (statusRes.tracking_url)

status :: Org.Organization -> API.StatusReq -> Flow API.StatusRes
status org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  payeeDetails <- payee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  let orderId = req.message.order_id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId case_.shortId
  (orderDetails :: OrderDetails) <-
    case_.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallback "status" API.onStatusAPI context cbUrl $ do
    taskStatus <- getStatus dzBACreds conf (TaskId taskId)
    let order = orderDetails.order
    onStatusMessage <- mkOnStatusMessage (org.name) order payeeDetails taskStatus
    let updatedOrder = onStatusMessage.order
    let updatedOrderDetails = orderDetails & #order .~ updatedOrder
    updateCase (case_.id) updatedOrderDetails taskStatus case_
    return onStatusMessage
  where
    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails, udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

cancel :: Org.Organization -> API.CancelReq -> Flow API.CancelRes
cancel org req = do
  let oId = req.message.order.id
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id oId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId $ case_.shortId
  orderDetails <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallback "cancel" API.onCancelAPI context cbUrl $ do
    callCancelAPI dzBACreds conf (TaskId taskId)
    let updatedOrder = cancelOrder (orderDetails.order)
    let updatedOrderDetails = orderDetails & #order .~ updatedOrder
    updateCase case_.id updatedOrderDetails case_
    return $ API.CancelResMessage updatedOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: Id Case -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails}
      Storage.update caseId updatedCase

update :: Org.Organization -> API.UpdateReq -> Flow API.UpdateRes
update org req = do
  DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withCallback "update" API.onUpdateAPI context cbUrl $ do
    -- TODO: Dunzo doesnt have update
    throwError $ ActionNotSupported "update"

-- Helpers
getQuote :: DzBAConfig -> DunzoConfig -> QuoteReq -> Flow QuoteRes
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus :: DzBAConfig -> DunzoConfig -> TaskId -> Flow TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken :: DzBAConfig -> DunzoConfig -> Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

validateReturn :: Order -> Flow ()
validateReturn currOrder =
  when (currOrder._type == Just "RETURN") $ do
    prevOrderId <- currOrder.prev_order_id & fromMaybeM (InvalidRequest "Prev order id is null.")
    prevOrderCase <- Storage.findById (Id prevOrderId) >>= fromMaybeM CaseDoesNotExist
    (prevOrderDetails :: OrderDetails) <-
      prevOrderCase.udf1 >>= decodeFromText
        & fromMaybeM (InvalidRequest "Decode error.")
    let prevOrder = prevOrderDetails.order
    -- validating that the items which are returned should be a subset of items in the actual order.
    -- would fail when there are duplicates in current order items
    unless (null $ (Item.id <$> currOrder.items) List.\\ (Item.id <$> prevOrder.items)) $
      throwError (InvalidRequest "Invalid return order.")

withCallback :: WithBecknCallback api callback_success r
withCallback = withBecknCallback (Just HttpSig.signatureAuthManagerKey)

withCallbackMig :: WithBecknCallbackMig api callback_success r
withCallbackMig = withBecknCallbackMig (Just HttpSig.signatureAuthManagerKey)
