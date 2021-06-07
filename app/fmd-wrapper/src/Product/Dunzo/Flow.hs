{-# LANGUAGE OverloadedLabels #-}

{-# HLINT ignore "Reduce duplication" #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
import qualified EulerHS.Types as ET
import qualified ExternalAPI.Dunzo.Flow as API
import ExternalAPI.Dunzo.Types
import Product.Dunzo.Transform
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import qualified Types.Beckn.API.Cancel as API
import qualified Types.Beckn.API.Confirm as API
import qualified Types.Beckn.API.Init as API
import qualified Types.Beckn.API.Search as API
import qualified Types.Beckn.API.Select as API
import qualified Types.Beckn.API.Status as API
import qualified Types.Beckn.API.Track as API
import qualified Types.Beckn.API.Update as API
import Types.Beckn.DecimalValue (convertDecimalValueToAmount)
import qualified Types.Beckn.FmdItem as Item
import Types.Beckn.FmdOrder
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common

search :: Org.Organization -> API.SearchReq -> Flow API.SearchRes
search org req = do
  config@DunzoConfig {..} <- dzConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  bapUrl <- context ^. #bap_uri & fromMaybeM (InvalidRequest "You should pass bap uri.")
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM OrgDoesNotExist
  dzBACreds <- getDzBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dzBACreds config quoteReq
    logTagInfo (req ^. #context . #transaction_id <> "_QuoteRes") $ show eres
    sendCb context eres
  return Ack
  where
    sendCb context res = do
      cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          logTagInfo (req ^. #context . #transaction_id <> "_on_search req") $ encodeToText onSearchReq
          onSearchResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onSearchAPI onSearchReq) "search"
          logTagInfo (req ^. #context . #transaction_id <> "_on_search res") $ show onSearchResp
        Left err -> do
          let onSearchErrReq = mkOnSearchErrReq context err
          logTagInfo (req ^. #context . #transaction_id <> "_on_search err req") $ encodeToText onSearchErrReq
          onSearchResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onSearchAPI onSearchErrReq) "search"
          logTagInfo (req ^. #context . #transaction_id <> "_on_search err res") $ show onSearchResp

select :: Org.Organization -> API.SelectReq -> Flow API.SelectRes
select org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let ctx = updateBppUri (req ^. #context) dzBPNwAddress
  validateOrderRequest $ req ^. #message . #order
  validateReturn $ req ^. #message . #order
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  dzBACreds <- getDzBAPCreds org
  fork "Select" do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote dzBACreds conf quoteReq
    logTagInfo (req ^. #context . #transaction_id <> "_QuoteRes") $ show eres
    sendCallback ctx dzQuotationTTLinMin cbUrl eres
  return Ack
  where
    sendCallback context quotationTTLinMin cbUrl = \case
      Right quoteRes -> do
        let reqOrder = req ^. #message . #order
        onSelectMessage <- mkOnSelectOrder reqOrder quotationTTLinMin quoteRes
        let onSelectReq = mkOnSelectReq context onSelectMessage
        let order = onSelectMessage ^. #order
        -- onSelectMessage has quotation
        let quote = fromJust $ onSelectMessage ^. #order . #quotation
        let quoteId = quote ^. #id
        let orderDetails = OrderDetails order quote
        Storage.storeQuote quoteId orderDetails
        logTagInfo (req ^. #context . #transaction_id <> "_on_select req") $ encodeToText onSelectReq
        onSelectResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onSelectAPI onSelectReq) "select"
        logTagInfo (req ^. #context . #transaction_id <> "_on_select res") $ show onSelectResp
      Left err -> do
        let onSelectReq = mkOnSelectErrReq context err
        logTagInfo (req ^. #context . #transaction_id <> "_on_select err req") $ encodeToText onSelectReq
        onSelectResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onSelectAPI onSelectReq) "select"
        logTagInfo (req ^. #context . #transaction_id <> "_on_select err res") $ show onSelectResp

    validateOrderRequest order = do
      let tasks = order ^. #tasks
      when (length tasks /= 1) $ throwError (InvalidRequest "Currently processing only one task per order.")
      let task = head tasks
      let package = task ^. #package
      let pickup = task ^. #pickup
      let drop = task ^. #drop
      when (isJust $ pickup ^. #time) $ throwError $ InvalidRequest "Scheduled pickup not supported."
      when (isJust $ drop ^. #time) $ throwError $ InvalidRequest "Scheduled drop not supported."
      void $ case readMaybe . T.unpack =<< (package ^. #package_category_id) of
        Nothing -> throwError $ InvalidRequest "Invalid package category id."
        -- Category id is the index value of dzPackageContentList
        Just cid ->
          unless (cid > 0 && cid <= length dzPackageContentList) $
            throwError $ InvalidRequest "Invalid package category id."

init :: Org.Organization -> API.InitReq -> Flow API.InitRes
init org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  quote <- req ^. (#message . #order . #quotation) & fromMaybeErr "INVALID_QUOTATION" (Just CORE003)
  let quoteId = quote ^. #id
  payeeDetails <- payee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybeErr "INVALID_QUOTATION_ID" (Just CORE003)
  let order = orderDetails ^. #order
  validateReturn order
  dzBACreds <- getDzBAPCreds org
  fork "init" do
    quoteReq <- mkQuoteReqFromSelect $ API.SelectReq context (API.SelectOrder (orderDetails ^. #order))
    eres <- getQuote dzBACreds conf quoteReq
    logTagInfo (req ^. #context . #transaction_id <> "_QuoteRes") $ show eres
    sendCb orderDetails context cbUrl payeeDetails quoteId dzQuotationTTLinMin eres
  return Ack
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
      createCaseIfNotPresent (getId $ org ^. #id) (onInitMessage ^. #order) (orderDetails ^. #quote)
      logTagInfo (req ^. #context . #transaction_id <> "_on_init req") $ encodeToText onInitReq
      onInitResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onInitAPI onInitReq) "init"
      logTagInfo (req ^. #context . #transaction_id <> "_on_init res") $ show onInitResp
    sendCb _ context cbUrl _ _ _ (Left err) = do
      let onInitReq = mkOnInitErrReq context err
      logTagInfo (req ^. #context . #transaction_id <> "_on_init err req") $ encodeToText onInitReq
      onInitResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onInitAPI onInitReq) "init"
      logTagInfo (req ^. #context . #transaction_id <> "_on_init err res") $ show onInitResp

    createCaseIfNotPresent orgId order quote = do
      now <- getCurrentTime
      let caseId = Id $ fromJust $ order ^. #id
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

confirm :: Org.Organization -> API.ConfirmReq -> Flow API.ConfirmRes
confirm org req = do
  dconf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybeErr "INVALID_ORDER_ID" (Just CORE003) $ reqOrder ^. #id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  (orderDetails :: OrderDetails) <- case_ ^. #udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let order = orderDetails ^. #order
  validateDelayFromInit dzQuotationTTLinMin case_
  verifyPayment reqOrder order
  validateReturn order
  payeeDetails <- payee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  txnId <-
    reqOrder ^? #payment . _Just . #transaction_id
      & fromMaybeErr "TXN_ID_NOT_PRESENT" Nothing
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #payment . _Just . #transaction_id) .~ txnId)
  dzBACreds <- getDzBAPCreds org
  fork "confirm" do
    createTaskReq <- mkCreateTaskReq order
    logTagInfo (req ^. #context . #transaction_id <> "_CreateTaskReq") (encodeToText createTaskReq)
    eres <- createTaskAPI dzBACreds dconf createTaskReq
    logTagInfo (req ^. #context . #transaction_id <> "_CreateTaskRes") $ show eres
    sendCb case_ updatedOrderDetailsWTxn context cbUrl payeeDetails eres
  return Ack
  where
    verifyPayment :: Order -> Order -> Flow ()
    verifyPayment reqOrder order = do
      confirmAmount <-
        reqOrder ^? #payment . _Just . #amount . #value
          & fromMaybeErr "INVALID_PAYMENT_AMOUNT" (Just CORE003)
      orderAmount <-
        order ^? #payment . _Just . #amount . #value
          & fromMaybeErr "ORDER_AMOUNT_NOT_FOUND" (Just CORE003)
      if confirmAmount == orderAmount
        then pass
        else throwError (InvalidRequest "Invalid order amount.")

    updateCase case_ orderDetails taskStatus = do
      let caseId = case_ ^. #id
      let taskId = taskStatus ^. #task_id
      let updatedCase =
            case_
              { shortId = getTaskId taskId,
                udf1 = Just $ encodeToText orderDetails,
                udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    sendCb case_ orderDetails context cbUrl payeeDetails = \case
      Right taskStatus -> do
        currTime <- getCurrentTime
        let uOrder = updateOrder (org ^. #name) currTime (orderDetails ^. #order) payeeDetails taskStatus
        checkAndLogPriceDiff (orderDetails ^. #order) uOrder
        updateCase case_ (orderDetails & #order .~ uOrder) taskStatus
        onConfirmReq <- mkOnConfirmReq context uOrder
        logTagInfo (req ^. #context . #transaction_id <> "_on_confirm req") $ encodeToText onConfirmReq
        eres <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onConfirmAPI onConfirmReq) "confirm"
        logTagInfo (req ^. #context . #transaction_id <> "_on_confirm res") $ show eres
      Left err -> do
        let onConfirmReq = mkOnConfirmErrReq context err
        logTagInfo (req ^. #context . #transaction_id <> "_on_confirm err req") $ encodeToText onConfirmReq
        onConfirmResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onConfirmAPI onConfirmReq) "confirm"
        logTagInfo (req ^. #context . #transaction_id <> "_on_confirm err res") $ show onConfirmResp

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder ^. #id
      let initPrice = convertDecimalValueToAmount . (^. #amount . #value) =<< initOrder ^. #payment
      let confirmPrice = convertDecimalValueToAmount . (^. #amount . #value) =<< confirmOrder ^. #payment
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

    validateDelayFromInit dzQuotationTTLinMin case_ = do
      now <- getCurrentTime
      let orderCreatedAt = case_ ^. #createdAt
      let thresholdTime = addUTCTime (fromInteger (dzQuotationTTLinMin * 60)) orderCreatedAt
      when (thresholdTime < now) $
        throwError (InvalidRequest "Took too long to confirm.")

track :: Org.Organization -> API.TrackReq -> Flow API.TrackRes
track org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let orderId = req ^. (#message . #order_id)
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  fork "track" do
    let taskId = case_ ^. #shortId
    dzBACreds <- getDzBAPCreds org
    eStatusRes <- getStatus dzBACreds conf (TaskId taskId)
    logTagInfo "StatusRes" $ show eStatusRes
    case eStatusRes of
      Left _ -> do
        let onTrackErrReq = mkOnTrackErrReq context "Failed to fetch tracking URL"
        logTagInfo (req ^. #context . #transaction_id <> "_on_track err req") $ encodeToText onTrackErrReq
        eres <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onTrackAPI onTrackErrReq) "track"
        logTagInfo (req ^. #context . #transaction_id <> "_on_track err res") $ show eres
      Right statusRes -> do
        let onTrackReq = mkOnTrackReq context orderId (statusRes ^. #tracking_url)
        logTagInfo (req ^. #context . #transaction_id <> "_on_track req") $ encodeToText onTrackReq
        eres <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onTrackAPI onTrackReq) "track"
        logTagInfo (req ^. #context . #transaction_id <> "_on_track res") $ show eres
  return Ack

status :: Org.Organization -> API.StatusReq -> Flow API.StatusRes
status org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  payeeDetails <- payee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  let orderId = req ^. (#message . #order_id)
  c <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = c ^. #shortId
  (orderDetails :: OrderDetails) <-
    c ^. #udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  fork "status" do
    eres <- getStatus dzBACreds conf (TaskId taskId)
    logTagInfo (req ^. #context . #transaction_id <> "_StatusRes") $ show eres
    sendCb c orderDetails context cbUrl payeeDetails eres
  return Ack
  where
    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails, udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

    callCbAPI cbUrl = callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl . ET.client API.onStatusAPI

    sendCb case_ orderDetails context cbUrl payeeDetails res = do
      let order = orderDetails ^. #order
      case res of
        Right taskStatus -> do
          onStatusMessage <- mkOnStatusMessage (org ^. #name) order payeeDetails taskStatus
          onStatusReq <- mkOnStatusReq context onStatusMessage
          let updatedOrder = onStatusMessage ^. #order
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #id) updatedOrderDetails taskStatus case_
          logTagInfo (req ^. #context . #transaction_id <> "_on_status req") $ encodeToText onStatusReq
          onStatusRes <- callCbAPI cbUrl onStatusReq "status"
          logTagInfo (req ^. #context . #transaction_id <> "_on_status res") $ show onStatusRes
        Left err -> do
          let onStatusReq = mkOnStatusErrReq context err
          logTagInfo (req ^. #context . #transaction_id <> "_on_status err req") $ encodeToText onStatusReq
          onStatusResp <- callCbAPI cbUrl onStatusReq "status"
          logTagInfo (req ^. #context . #transaction_id <> "_on_status err res") $ show onStatusResp

cancel :: Org.Organization -> API.CancelReq -> Flow API.CancelRes
cancel org req = do
  let oId = req ^. (#message . #order . #id)
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id oId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = case_ ^. #shortId
  orderDetails <- case_ ^. #udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  fork "cancel" do
    eres <- callCancelAPI dzBACreds conf (TaskId taskId)
    logTagInfo (req ^. #context . #transaction_id <> "_CancelRes") $ show eres
    sendCb case_ orderDetails context cbUrl eres
  return Ack
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: Id Case -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails}
      Storage.update caseId updatedCase

    sendCb case_ orderDetails context cbUrl = \case
      Right () -> do
        let updatedOrder = cancelOrder (orderDetails ^. #order)
        onCancelReq <- mkOnCancelReq context updatedOrder
        let updatedOrderDetails = orderDetails & #order .~ updatedOrder
        updateCase (case_ ^. #id) updatedOrderDetails case_
        logTagInfo (req ^. #context . #transaction_id <> "_on_cancel req") $ encodeToText onCancelReq
        onCancelRes <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onCancelAPI onCancelReq) "cancel"
        logTagInfo (req ^. #context . #transaction_id <> "_on_cancel res") $ show onCancelRes
      Left err -> do
        let onCancelReq = mkOnCancelErrReq context err
        logTagInfo (req ^. #context . #transaction_id <> "_on_cancel err req") $ encodeToText onCancelReq
        onCancelResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl (ET.client API.onCancelAPI onCancelReq) "cancel"
        logTagInfo (req ^. #context . #transaction_id <> "_on_cancel err res") $ show onCancelResp

update :: Org.Organization -> API.UpdateReq -> Flow API.UpdateRes
update org req = do
  DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  fork "update" do
    -- TODO: Dunzo doesnt have update
    let onUpdateReq = mkOnUpdateErrReq context
    logTagInfo (req ^. #context . #transaction_id <> "_on_update err req") $ encodeToText onUpdateReq
    eres <- callAPI cbUrl (ET.client API.onUpdateAPI onUpdateReq) "update"
    logTagInfo (req ^. #context . #transaction_id <> "_on_update err res") $ show eres
  return Ack

-- Helpers
getQuote :: DzBAConfig -> DunzoConfig -> QuoteReq -> Flow (Either Error QuoteRes)
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus :: DzBAConfig -> DunzoConfig -> TaskId -> Flow (Either Error TaskStatus)
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken :: DzBAConfig -> DunzoConfig -> Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <-
        API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
          >>= liftEither
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

validateReturn :: Order -> Flow ()
validateReturn currOrder =
  when (currOrder ^. #_type == Just "RETURN") $ do
    prevOrderId <- currOrder ^. #prev_order_id & fromMaybeM (InvalidRequest "Prev order id is null.")
    prevOrderCase <- Storage.findById (Id prevOrderId) >>= fromMaybeM CaseDoesNotExist
    (prevOrderDetails :: OrderDetails) <-
      prevOrderCase ^. #udf1 >>= decodeFromText
        & fromMaybeM (InvalidRequest "Decode error.")
    let prevOrder = prevOrderDetails ^. #order
    -- validating that the items which are returned should be a subset of items in the actual order.
    -- would fail when there are duplicates in current order items
    unless (null $ (Item.id <$> currOrder ^. #items) List.\\ (Item.id <$> prevOrder ^. #items)) $
      throwError (InvalidRequest "Invalid return order.")
