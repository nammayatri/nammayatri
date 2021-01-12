{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# HLINT ignore "Reduce duplication" #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.App (CaseId (..), _getOrganizationId)
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.FMD.API.Cancel as API
import qualified Beckn.Types.FMD.API.Confirm as API
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Select as API
import qualified Beckn.Types.FMD.API.Status as API
import qualified Beckn.Types.FMD.API.Track as API
import qualified Beckn.Types.FMD.API.Update as API
import qualified Beckn.Types.FMD.Item as Item
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (decodeFromText, encodeToText, fork, fromMaybeM400, fromMaybeM500, getCurrTime, throwBecknError400, throwBecknError500)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (addUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (drop)
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common (fromMaybe400Log)

search :: Org.Organization -> API.SearchReq -> Flow API.SearchRes
search org req = do
  config@DunzoConfig {..} <- dzConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  bapUrl <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_URI"
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM400 "BAP_NOT_CONFIGURED"
  dzBACreds <- getDzBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dzBACreds config quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCb context eres
  returnAck context
  where
    sendCb context res = do
      cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search req") $ encodeToText onSearchReq
          onSearchResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSearchAPI onSearchReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search res") $ show onSearchResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSearchErrReq = mkOnSearchErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err req") $ encodeToText onSearchErrReq
              onSearchResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSearchAPI onSearchErrReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err res") $ show onSearchResp
        _ -> pass

select :: Org.Organization -> API.SelectReq -> Flow API.SelectRes
select org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let ctx = updateBppUri (req ^. #context) dzBPNwAddress
  validateOrderRequest $ req ^. #message . #order
  validateReturn $ req ^. #message . #order
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  dzBACreds <- getDzBAPCreds org
  fork "Select" do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote dzBACreds conf quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCallback ctx dzQuotationTTLinMin cbUrl eres
  returnAck ctx
  where
    sendCallback context quotationTTLinMin cbUrl res =
      case res of
        Right quoteRes -> do
          let reqOrder = req ^. #message . #order
          onSelectMessage <- mkOnSelectOrder reqOrder quotationTTLinMin quoteRes
          let onSelectReq = mkOnSelectReq context onSelectMessage
          let order = onSelectMessage ^. #order
          -- onSelectMessage has quotation
          let quote = fromJust $ onSelectMessage ^. #order . #_quotation
          let quoteId = quote ^. #_id
          let orderDetails = OrderDetails order quote
          Storage.storeQuote quoteId orderDetails
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select req") $ encodeToText onSelectReq
          onSelectResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSelectAPI onSelectReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select res") $ show onSelectResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSelectReq = mkOnSelectErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select err req") $ encodeToText onSelectReq
              onSelectResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSelectAPI onSelectReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select err res") $ show onSelectResp
        _ -> pass

    validateOrderRequest order = do
      let tasks = order ^. #_tasks
      when (length tasks /= 1) $ throwBecknError400 "CURRENTLY_PROCESSING_ONLY_ONE_TASK_PER_ORDER"
      let task = head tasks
      let package = task ^. #_package
      let pickup = task ^. #_pickup
      let drop = task ^. #_drop
      when (isJust $ pickup ^. #_time) $ throwBecknError400 "SCHEDULED_PICKUP_NOT_SUPPORTED"
      when (isJust $ drop ^. #_time) $ throwBecknError400 "SCHEDULED_DROP_NOT_SUPPORTED"
      void $ case readMaybe . T.unpack =<< (package ^. #_package_category_id) of
        Nothing -> throwBecknError400 "INVALID_PACKAGE_CATEGORY_ID"
        -- Category id is the index value of dzPackageContentList
        Just cid -> unless (cid > 0 && cid <= length dzPackageContentList) $ throwBecknError400 "INVALID_PACKAGE_CATEGORY_ID"

init :: Org.Organization -> API.InitReq -> Flow API.InitRes
init org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  quote <- req ^. (#message . #order . #_quotation) & fromMaybe400Log "INVALID_QUOTATION" (Just CORE003) context
  let quoteId = quote ^. #_id
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybe400Log "INVALID_QUOTATION_ID" (Just CORE003) context
  let order = orderDetails ^. #order
  validateReturn order
  dzBACreds <- getDzBAPCreds org
  fork "init" do
    quoteReq <- mkQuoteReqFromSelect $ API.SelectReq context (API.SelectOrder (orderDetails ^. #order))
    eres <- getQuote dzBACreds conf quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCb orderDetails context cbUrl payeeDetails quoteId dzQuotationTTLinMin eres
  returnAck context
  where
    sendCb orderDetails context cbUrl payeeDetails quoteId quotationTTLinMin (Right res) = do
      -- quoteId will be used as orderId
      onInitMessage <-
        mkOnInitMessage
          quoteId
          quotationTTLinMin
          (orderDetails ^. #order)
          payeeDetails
          req
          res
      let onInitReq = mkOnInitReq context onInitMessage
      createCaseIfNotPresent (_getOrganizationId $ org ^. #_id) (onInitMessage ^. #order) (orderDetails ^. #quote)
      L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init req") $ encodeToText onInitReq
      onInitResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onInitAPI onInitReq
      L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init res") $ show onInitResp
      return ()
    sendCb _ context cbUrl _ _ _ (Left (FailureResponse _ (Response _ _ _ body))) = do
      case decode body of
        Just err -> do
          let onInitReq = mkOnInitErrReq context err
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init err req") $ encodeToText onInitReq
          onInitResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onInitAPI onInitReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init err res") $ show onInitResp
          return ()
        Nothing -> return ()
    sendCb _ _ _ _ _ _ _ = return ()

    createCaseIfNotPresent orgId order quote = do
      now <- getCurrTime
      let caseId = CaseId $ fromJust $ order ^. #_id
      let case_ =
            Case
              { _id = caseId,
                _name = Nothing,
                _description = Nothing,
                _shortId = "", -- FIX this
                _industry = GROCERY,
                _type = RIDEORDER,
                _exchangeType = ORDER,
                _status = NEW,
                _startTime = now,
                _endTime = Nothing,
                _validTill = now,
                _provider = Just "Dunzo",
                _providerType = Nothing,
                _requestor = Just orgId,
                _requestorType = Nothing,
                _parentCaseId = Nothing,
                _fromLocationId = "",
                _toLocationId = "",
                _udf1 = Just $ encodeToText (OrderDetails order quote),
                _udf2 = Nothing,
                _udf3 = Nothing,
                _udf4 = Nothing,
                _udf5 = Nothing,
                _info = Nothing,
                _createdAt = now,
                _updatedAt = now
              }
      mcase <- Storage.findById caseId
      case mcase of
        Nothing -> Storage.create case_
        Just _ -> pass

confirm :: Org.Organization -> API.ConfirmReq -> Flow API.ConfirmRes
confirm org req = do
  dconf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybe400Log "INVALID_ORDER_ID" (Just CORE003) context $ reqOrder ^. #_id
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  (orderDetails :: OrderDetails) <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let order = orderDetails ^. #order
  validateDelayFromInit dzQuotationTTLinMin case_
  verifyPayment reqOrder order
  validateReturn order
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  txnId <-
    reqOrder ^? #_payment . _Just . #_transaction_id
      & fromMaybe400Log "TXN_ID_NOT_FOUND" Nothing context
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #_payment . _Just . #_transaction_id) .~ txnId)
  dzBACreds <- getDzBAPCreds org
  fork "confirm" do
    createTaskReq <- mkCreateTaskReq context order
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_CreateTaskReq") (encodeToText createTaskReq)
    eres <- createTaskAPI dzBACreds dconf createTaskReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_CreateTaskRes") $ show eres
    sendCb case_ updatedOrderDetailsWTxn context cbUrl payeeDetails eres
  returnAck context
  where
    verifyPayment :: Order -> Order -> Flow ()
    verifyPayment reqOrder order = do
      let context = req ^. #context
      confirmAmount <-
        reqOrder ^? #_payment . _Just . #_amount . #_value
          & fromMaybe400Log "INVALID_PAYMENT_AMOUNT" (Just CORE003) context
      orderAmount <-
        order ^? #_payment . _Just . #_amount . #_value
          & fromMaybe400Log "ORDER_AMOUNT_NOT_FOUND" (Just CORE003) context
      if confirmAmount == orderAmount
        then pass
        else throwBecknError400 "INVALID_ORDER_AMOUNT"

    updateCase case_ orderDetails taskStatus = do
      let caseId = case_ ^. #_id
      let taskId = taskStatus ^. #task_id
      let updatedCase =
            case_
              { _shortId = getTaskId taskId,
                _udf1 = Just $ encodeToText orderDetails,
                _udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    sendCb case_ orderDetails context cbUrl payeeDetails res = do
      case res of
        Right taskStatus -> do
          currTime <- getCurrTime
          let uOrder = updateOrder (org ^. #_name) currTime (orderDetails ^. #order) payeeDetails taskStatus
          checkAndLogPriceDiff (orderDetails ^. #order) uOrder
          updateCase case_ (orderDetails & #order .~ uOrder) taskStatus
          onConfirmReq <- mkOnConfirmReq context uOrder
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm req") $ encodeToText onConfirmReq
          eres <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onConfirmAPI onConfirmReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm res") $ show eres
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onConfirmReq = mkOnConfirmErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm err req") $ encodeToText onConfirmReq
              onConfirmResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onConfirmAPI onConfirmReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm err res") $ show onConfirmResp
        _ -> pass

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder ^. #_id
      let initPrice = convertDecimalValueToAmount . (^. #_amount . #_value) =<< initOrder ^. #_payment
      let confirmPrice = convertDecimalValueToAmount . (^. #_amount . #_value) =<< confirmOrder ^. #_payment
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            L.logInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

    validateDelayFromInit dzQuotationTTLinMin case_ = do
      now <- getCurrTime
      let orderCreatedAt = case_ ^. #_createdAt
      let thresholdTime = addUTCTime (fromInteger (dzQuotationTTLinMin * 60)) orderCreatedAt
      when (thresholdTime < now) $
        throwBecknError400 "TOOK_TOO_LONG_TO_CONFIRM"

track :: Org.Organization -> API.TrackReq -> Flow API.TrackRes
track org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let orderId = req ^. (#message . #order_id)
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  fork "track" do
    let taskId = case_ ^. #_shortId
    dzBACreds <- getDzBAPCreds org
    eStatusRes <- getStatus dzBACreds conf (TaskId taskId)
    L.logInfo @Text "StatusRes" $ show eStatusRes
    case eStatusRes of
      Left _ -> do
        let onTrackErrReq = mkOnTrackErrReq context "Failed to fetch tracking URL"
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_track err req") $ encodeToText onTrackErrReq
        eres <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onTrackAPI onTrackErrReq
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_track err res") $ show eres
      Right statusRes -> do
        let onTrackReq = mkOnTrackReq context orderId (statusRes ^. #tracking_url)
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_track req") $ encodeToText onTrackReq
        eres <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onTrackAPI onTrackReq
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_track res") $ show eres
  returnAck context

status :: Org.Organization -> API.StatusReq -> Flow API.StatusRes
status org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  let orderId = req ^. (#message . #order_id)
  c <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let taskId = c ^. #_shortId
  (orderDetails :: OrderDetails) <- c ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  dzBACreds <- getDzBAPCreds org
  fork "status" do
    eres <- getStatus dzBACreds conf (TaskId taskId)
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_StatusRes") $ show eres
    sendCb c orderDetails context cbUrl payeeDetails eres
  returnAck context
  where
    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {_udf1 = Just $ encodeToText orderDetails, _udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

    callCbAPI cbUrl = L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl . ET.client API.onStatusAPI

    sendCb case_ orderDetails context cbUrl payeeDetails res = do
      let order = orderDetails ^. #order
      case res of
        Right taskStatus -> do
          onStatusMessage <- mkOnStatusMessage (org ^. #_name) order payeeDetails taskStatus
          onStatusReq <- mkOnStatusReq context onStatusMessage
          let updatedOrder = onStatusMessage ^. #order
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #_id) updatedOrderDetails taskStatus case_
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_status req") $ encodeToText onStatusReq
          onStatusRes <- callCbAPI cbUrl onStatusReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_status res") $ show onStatusRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onStatusReq = mkOnStatusErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_status err req") $ encodeToText onStatusReq
              onStatusResp <- callCbAPI cbUrl onStatusReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_status err res") $ show onStatusResp
        _ -> pass

cancel :: Org.Organization -> API.CancelReq -> Flow API.CancelRes
cancel org req = do
  let oId = req ^. (#message . #order . #id)
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  case_ <- Storage.findById (CaseId oId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let taskId = case_ ^. #_shortId
  orderDetails <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  dzBACreds <- getDzBAPCreds org
  fork "cancel" do
    eres <- callCancelAPI dzBACreds conf (TaskId taskId)
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_CancelRes") $ show eres
    sendCb case_ orderDetails context cbUrl eres
  returnAck context
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: CaseId -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedCase = case_ {_udf1 = Just $ encodeToText orderDetails}
      Storage.update caseId updatedCase

    sendCb case_ orderDetails context cbUrl res =
      case res of
        Right () -> do
          let updatedOrder = cancelOrder (orderDetails ^. #order)
          onCancelReq <- mkOnCancelReq context updatedOrder
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #_id) updatedOrderDetails case_
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_cancel req") $ encodeToText onCancelReq
          onCancelRes <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onCancelAPI onCancelReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_cancel res") $ show onCancelRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onCancelReq = mkOnCancelErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_cancel err req") $ encodeToText onCancelReq
              onCancelResp <- L.callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onCancelAPI onCancelReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_cancel err res") $ show onCancelResp
        _ -> pass

update :: Org.Organization -> API.UpdateReq -> Flow API.UpdateRes
update org req = do
  DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  fork "update" do
    -- TODO: Dunzo doesnt have update
    let onUpdateReq = mkOnUpdateErrReq context
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_update err req") $ encodeToText onUpdateReq
    eres <- L.callAPI cbUrl $ ET.client API.onUpdateAPI onUpdateReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_update err res") $ show eres
  returnAck context

-- Helpers
getQuote :: DzBAConfig -> DunzoConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus :: DzBAConfig -> DunzoConfig -> TaskId -> Flow (Either ClientError TaskStatus)
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken :: DzBAConfig -> DunzoConfig -> Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      eres <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      case eres of
        Left err -> throwBecknError500 $ show err
        Right (TokenRes token) -> do
          Dz.insertToken dzClientId token
          return token
    Just token -> return token

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack "ACK") Nothing

validateReturn :: Order -> Flow ()
validateReturn currOrder =
  when (currOrder ^. #_type == Just "RETURN") $ do
    prevOrderId <- currOrder ^. #_prev_order_id & fromMaybeM400 "INVALID_ORDER_ID"
    prevOrderCase <- Storage.findById (CaseId prevOrderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
    (prevOrderDetails :: OrderDetails) <- prevOrderCase ^. #_udf1 >>= decodeFromText & fromMaybeM400 "ORDER_NOT_FOUND"
    let prevOrder = prevOrderDetails ^. #order
    -- validating that the items which are returned should be a subset of items in the actual order.
    -- would fail when there are duplicates in current order items
    unless (null $ (Item._id <$> currOrder ^. #_items) List.\\ (Item._id <$> prevOrder ^. #_items)) $
      throwBecknError400 "INVALID_RETURN_ORDER"
