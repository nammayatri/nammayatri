{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.App (CaseId (..), _getOrganizationId)
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes, onCancelAPI)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes, onConfirmAPI)
import Beckn.Types.FMD.API.Init (InitReq, InitRes, onInitAPI)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (DraftOrder (..), SelectReq (..), SelectRes, onSelectAPI)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes, onStatusAPI)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes, onTrackAPI)
import Beckn.Types.FMD.API.Update (UpdateReq, UpdateRes, onUpdateAPI)
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (decodeFromText, encodeToText, fork, fromMaybeM400, fromMaybeM500, getCurrTime, throwJsonError400, throwJsonError500)
import Control.Lens ((?~))
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Quote as Storage
import Types.Wrapper
import Utils.Common (parseBaseUrl)

search :: Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DunzoConfig {..} <- getDunzoConfig org
  quoteReq <- mkQuoteReqFromSearch req
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  fork "Search" $ do
    eres <- getQuote config quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    case eres of
      Left err ->
        case err of
          FailureResponse _ (Response _ _ _ body) -> sendErrCb org context body
          _ -> L.logDebug @Text "getQuoteErr" (show err)
      Right res -> sendCb org context res
  returnAck context
  where
    sendCb org' context res = do
      onSearchReq <- mkOnSearchReq org' context res
      cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
      cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      cbres <- callCbAPI cbApiKey cbUrl onSearchReq
      L.logDebug @Text "cb" $
        decodeUtf8 (encode onSearchReq)
          <> show cbres

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onSearchAPI cbApiKey

    sendErrCb org' context errbody =
      case decode errbody of
        Just err -> do
          cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
          cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
          let onSearchErrReq = mkOnSearchErrReq context err
          cbres <- callCbAPI cbApiKey cbUrl onSearchErrReq
          L.logDebug @Text "cb" $
            decodeUtf8 (encode onSearchErrReq)
              <> show cbres
        Nothing -> L.logDebug @Text "getQuoteErr" "UNABLE_TO_DECODE_ERR"

select :: Organization -> SelectReq -> Flow SelectRes
select org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  ctx <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  bapNwAddr <- ctx ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  fork "Select" do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote conf quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    sendCallback ctx eres baConfig
  returnAck ctx
  where
    sendCallback context res baConfig = do
      let cbApiKey = baConfig ^. #bap_api_key
          bapNwAddr = baConfig ^. #bap_nw_address
      cbUrl <- parseBaseUrl bapNwAddr
      case res of
        Right quoteRes -> do
          quote <- mkQuote quoteRes
          let onSelectMessage = mkOnSelectMessage quote req
          let onSelectReq = mkOnSelectReq context onSelectMessage
          let quoteId = quote ^. #_id
          let order = onSelectMessage ^. #order
          let orderDetails = OrderDetails order quote
          Storage.storeQuote quoteId orderDetails
          L.logInfo @Text "on_select" $ "on_select cb req" <> show onSelectReq
          onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
          L.logInfo @Text "on_select" $ "on_select cb resp" <> show onSelectResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSelectReq = mkOnSelectErrReq context err
              L.logInfo @Text "on_select" $ "on_select cb err req" <> show onSelectReq
              onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
              L.logInfo @Text "on_select" $ "on_select cb err resp" <> show onSelectResp
        _ -> pass

init :: Organization -> InitReq -> Flow InitRes
init org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  quote <- req ^. (#message . #order . #_quotation) & fromMaybeM400 "INVALID_QUOTATION"
  let quoteId = quote ^. #_id
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybeM400 "INVALID_QUOTATION_ID"
  fork "init" do
    quoteReq <- mkQuoteReqFromSelect $ SelectReq context (DraftOrder (orderDetails ^. #order))
    eres <- getQuote conf quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    sendCb orderDetails context conf baConfig quoteId eres
  returnAck context
  where
    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onInitAPI cbApiKey

    sendCb orderDetails context conf baConfig quoteId (Right res) = do
      let cbApiKey = baConfig ^. #bap_api_key
          bapNwAddr = baConfig ^. #bap_nw_address
      cbUrl <- parseBaseUrl bapNwAddr
      -- quoteId will be used as orderId
      let onInitMessage =
            mkOnInitMessage
              quoteId
              (orderDetails ^. #order)
              conf
              req
              res
      let onInitReq = mkOnInitReq context onInitMessage
      createCaseIfNotPresent (_getOrganizationId $ org ^. #_id) bapNwAddr (onInitMessage ^. #order) (orderDetails ^. #quote)
      onInitResp <- callCbAPI cbApiKey cbUrl onInitReq
      L.logInfo @Text "on_init" $ show onInitResp
      return ()
    sendCb _ context _ baConfig _ (Left (FailureResponse _ (Response _ _ _ body))) = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      case decode body of
        Just err -> do
          let onInitReq = mkOnInitErrReq context err
          onInitResp <- callCbAPI cbApiKey cbUrl onInitReq
          L.logInfo @Text "on_init err" $ show onInitResp
          return ()
        Nothing -> return ()
    sendCb _ _ _ _ _ _ = return ()

    createCaseIfNotPresent orgId bapUrl order quote = do
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
                _provider = Just orgId,
                _providerType = Nothing,
                _requestor = Just bapUrl,
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

confirm :: Organization -> ConfirmReq -> Flow ConfirmRes
confirm org req = do
  dconf@DunzoConfig {..} <- getDunzoConfig org
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  bapConfig <- getBAConfig bapNwAddr dconf
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybeM400 "INVALID_ORDER_ID" $ reqOrder ^. #_id
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  (orderDetails :: OrderDetails) <- case_ ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  let order = orderDetails ^. #order
  verifyPayment reqOrder order
  txnId <-
    reqOrder ^? #_payment . _Just . #_transaction_id
      & fromMaybeM400 "TXN_ID_NOT_FOUND"
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #_payment . _Just . #_transaction_id) .~ txnId)
  fork "confirm" do
    L.logInfo @Text "Confirm" "Started"
    createTaskReq <- mkCreateTaskReq order
    L.logInfo @Text "CreateTaskReq" (encodeToText createTaskReq)
    eres <- createTaskAPI dconf createTaskReq
    L.logInfo @Text "CreateTaskRes" $ show eres
    sendCb case_ updatedOrderDetailsWTxn context bapConfig eres
  returnAck context
  where
    verifyPayment :: Order -> Order -> Flow ()
    verifyPayment reqOrder order = do
      confirmAmount <-
        reqOrder ^? #_payment . _Just . #_amount . #_value
          & fromMaybeM400 "INVALID_PAYMENT_AMOUNT"
      orderAmount <-
        order ^? #_payment . _Just . #_amount . #_value
          & fromMaybeM500 "ORDER_AMOUNT_NOT_FOUND"
      if confirmAmount == orderAmount
        then pass
        else throwJsonError400 "AMOUNT_VALIDATION_ERR" "INVALID_ORDER_AMOUNT"

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

    createTaskAPI conf@DunzoConfig {..} req' = do
      baseUrl <- parseBaseUrl dzUrl
      token <- fetchToken conf
      API.createTask dzClientId token baseUrl req'

    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onConfirmAPI cbApiKey req'

    sendCb case_ orderDetails context baConfig res =
      case res of
        Right taskStatus -> do
          currTime <- getCurrTime
          let uOrder = updateOrder (org ^. #_name) currTime (orderDetails ^. #order) taskStatus
          updateCase case_ (orderDetails & #order .~ uOrder) taskStatus
          onConfirmReq <- mkOnConfirmReq context uOrder
          eres <- callCbAPI baConfig onConfirmReq
          L.logInfo @Text "on_confirm" $ show eres
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onConfirmReq = mkOnConfirmErrReq context err
              onConfirmResp <- callCbAPI baConfig onConfirmReq
              L.logInfo @Text "on_confirm err " $ show onConfirmResp
        _ -> pass

track :: Organization -> TrackReq -> Flow TrackRes
track org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  let orderId = req ^. (#message . #order_id)
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  void $ Storage.findById (CaseId orderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  fork "track" do
    -- TODO: fix this after dunzo sends tracking url in api
    let onTrackReq = mkOnTrackErrReq context
    eres <- callCbAPI baConfig onTrackReq
    L.logInfo @Text "on_track" $ show eres
  returnAck context
  where
    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onTrackAPI cbApiKey req'

status :: Organization -> StatusReq -> Flow StatusRes
status org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  let orderId = req ^. (#message . #order_id)
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  c <- Storage.findById (CaseId orderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  let taskId = c ^. #_shortId
  (orderDetails :: OrderDetails) <- c ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  fork "status" do
    eres <- getStatus conf (TaskId taskId)
    L.logInfo @Text "StatusRes" $ show eres
    sendCb c orderDetails context baConfig eres
  returnAck context
  where
    getStatus conf@DunzoConfig {..} taskId = do
      baseUrl <- parseBaseUrl dzUrl
      token <- fetchToken conf
      API.taskStatus dzClientId token baseUrl taskId

    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {_udf1 = Just $ encodeToText orderDetails, _udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onStatusAPI cbApiKey

    sendCb case_ orderDetails context baConfig res = do
      let cbApiKey = baConfig ^. #bap_api_key
      let order = orderDetails ^. #order
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      case res of
        Right taskStatus -> do
          onStatusMessage <- mkOnStatusMessage (org ^. #_name) order taskStatus
          onStatusReq <- mkOnStatusReq context onStatusMessage
          let updatedOrder = onStatusMessage ^. #order
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #_id) updatedOrderDetails taskStatus case_
          onStatusRes <- callCbAPI cbApiKey cbUrl onStatusReq
          L.logInfo @Text "on_status " $ show onStatusRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onStatusReq = mkOnStatusErrReq context err
              onStatusResp <- callCbAPI cbApiKey cbUrl onStatusReq
              L.logInfo @Text "on_status err " $ show onStatusResp
        _ -> pass

cancel :: Organization -> CancelReq -> Flow CancelRes
cancel org req = do
  let oId = req ^. (#message . #order . #id)
  conf@DunzoConfig {..} <- getDunzoConfig org
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  bapConfig <- getBAConfig bapNwAddr conf
  case_ <- Storage.findById (CaseId oId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  let taskId = case_ ^. #_shortId
  orderDetails <- case_ ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  fork "cancel" do
    eres <- callCancelAPI conf (TaskId taskId)
    L.logInfo @Text "CancelRes" $ show eres
    sendCb case_ orderDetails context bapConfig eres
  returnAck context
  where
    callCancelAPI conf@DunzoConfig {..} taskId = do
      baseUrl <- parseBaseUrl dzUrl
      token <- fetchToken conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token baseUrl taskId ""

    updateCase :: CaseId -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedOrderDetails = orderDetails & (#order . #_state) ?~ "CANCELLED"
          updatedCase = case_ {_udf1 = Just $ encodeToText updatedOrderDetails}
      Storage.update caseId updatedCase

    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onCancelAPI cbApiKey req'

    sendCb case_ orderDetails context baConfig res =
      case res of
        Right () -> do
          let order = orderDetails ^. #order
          onCancelReq <- mkOnCancelReq context order
          updateCase (case_ ^. #_id) orderDetails case_
          onCancelRes <- callCbAPI baConfig onCancelReq
          L.logInfo @Text "on_cancel " $ show onCancelRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onCancelReq = mkOnCancelErrReq context err
              onCancelResp <- callCbAPI baConfig onCancelReq
              L.logInfo @Text "on_cancel err " $ show onCancelResp
        _ -> pass

update :: Organization -> UpdateReq -> Flow UpdateRes
update org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  context <- updateVersions $ updateBppUri (req ^. #context) dzBPNwAddress
  bapNwAddr <- context ^. #_bap_uri & fromMaybeM400 "INVALID_AC_ID"
  baConfig <- getBAConfig bapNwAddr conf
  fork "update" do
    -- TODO: Dunzo doesnt have update
    let onUpdateReq = mkOnUpdateErrReq context
    eres <- callCbAPI baConfig onUpdateReq
    L.logInfo @Text "on_update" $ show eres
  returnAck context
  where
    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onUpdateAPI cbApiKey req'

-- Helpers
getQuote :: DunzoConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote conf@DunzoConfig {..} quoteReq = do
  baseUrl <- parseBaseUrl dzUrl
  token <- fetchToken conf
  API.getQuote dzClientId token baseUrl quoteReq

fetchToken :: DunzoConfig -> Flow Token
fetchToken DunzoConfig {..} = do
  tokenUrl <- parseBaseUrl "http://d4b.dunzodev.in:9016" -- TODO: Fix this, should not be hardcoded
  mToken <- Dz.getToken
  case mToken of
    Nothing -> do
      eres <- API.getToken tokenUrl (TokenReq dzClientId dzClientSecret)
      case eres of
        Left err -> throwJsonError500 "TOKEN_ERR" (show err)
        Right (TokenRes token) -> do
          Dz.insertToken token
          return token
    Just token -> return token

getBAConfig :: Text -> DunzoConfig -> Flow BAConfig
getBAConfig bapNwAddr DunzoConfig {..} =
  find (\c -> c ^. #bap_nw_address == bapNwAddr) dzBAConfigs
    & fromMaybeM500 "BAP_NOT_CONFIGURED"

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack "ACK") Nothing
