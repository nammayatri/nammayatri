{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Delhivery.Flow where

import App.Types
import Beckn.Types.App (CaseId (..), _getOrganizationId)
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes, onConfirmAPI)
import Beckn.Types.FMD.API.Init (InitReq, InitRes, onInitAPI)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (SelectOrder (..), SelectReq (..), SelectRes, onSelectAPI)
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Delhivery.Flow as API
import External.Delhivery.Types
import Product.Delhivery.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common

search :: Org.Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  bapUrl <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_URI"
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM400 "BAP_NOT_CONFIGURED"
  dlBACreds <- getDlBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dlBACreds config quoteReq
    sendCb context eres
  returnAck context
  where
    sendCb context res = do
      cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search req") $ encodeToText onSearchReq
          onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search res") $ show onSearchResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSearchErrReq = mkOnSearchErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err req") $ encodeToText onSearchErrReq
              onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchErrReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err res") $ show onSearchResp
        _ -> pass

select :: Org.Organization -> SelectReq -> Flow SelectRes
select org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  --  validateOrderRequest $ req ^. #message . #order
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  dlBACreds <- getDlBAPCreds org
  fork "Select" $ do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote dlBACreds config quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCallback context cbUrl cbApiKey eres
  returnAck context
  where
    sendCallback context cbUrl cbApiKey res =
      case res of
        Right quoteRes -> do
          let reqOrder = req ^. #message . #order
          onSelectMessage <- mkOnSelectOrder reqOrder quoteRes
          let onSelectReq = mkOnSelectReq context onSelectMessage
          let order = onSelectMessage ^. #order
          -- onSelectMessage has quotation
          let quote = fromJust $ onSelectMessage ^. #order . #_quotation
          let quoteId = quote ^. #_id
          let orderDetails = OrderDetails order quote
          Storage.storeQuote quoteId orderDetails
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select req") $ encodeToText onSelectReq
          onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select res") $ show onSelectResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSelectReq = mkOnSelectErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select err req") $ encodeToText onSelectReq
              onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_select err res") $ show onSelectResp
        _ -> pass

init :: Org.Organization -> InitReq -> Flow InitRes
init org req = do
  conf@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  quote <- req ^. (#message . #order . #_quotation) & fromMaybe400Log "INVALID_QUOTATION" (Just CORE003) context
  let quoteId = quote ^. #_id
  payeeDetails <- dlPayee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybe400Log "INVALID_QUOTATION_ID" (Just CORE003) context
  dlBACreds <- getDlBAPCreds org
  fork "init" $ do
    quoteReq <- mkQuoteReqFromSelect $ SelectReq context (SelectOrder (orderDetails ^. #order))
    eres <- getQuote dlBACreds conf quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCb orderDetails context cbApiKey cbUrl payeeDetails quoteId eres
  returnAck context
  where
    sendCb orderDetails context cbApiKey cbUrl payeeDetails quoteId (Right res) = do
      -- quoteId will be used as orderId
      onInitMessage <-
        mkOnInitMessage
          quoteId
          (orderDetails ^. #order)
          payeeDetails
          req
          res
      let onInitReq = mkOnInitReq context onInitMessage
      createCaseIfNotPresent (_getOrganizationId $ org ^. #_id) (onInitMessage ^. #order) (orderDetails ^. #quote)
      L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init req") $ encodeToText onInitReq
      onInitResp <- L.callAPI cbUrl $ ET.client onInitAPI cbApiKey onInitReq
      L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init res") $ show onInitResp
      return ()
    sendCb _ context cbApiKey cbUrl _ _ (Left (FailureResponse _ (Response _ _ _ body))) =
      case decode body of
        Just err -> do
          let onInitReq = mkOnInitErrReq context err
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_init err req") $ encodeToText onInitReq
          onInitResp <- L.callAPI cbUrl $ ET.client onInitAPI cbApiKey onInitReq
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
                _provider = Just "Delhivery",
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

confirm :: Org.Organization -> ConfirmReq -> Flow ConfirmRes
confirm org req = do
  dconf@DelhiveryConfig {..} <- dlConfig <$> ask
  let ctx = updateBppUri (req ^. #context) dlBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybe400Log "INVALID_ORDER_ID" (Just CORE003) ctx $ reqOrder ^. #_id
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) ctx
  (orderDetails :: OrderDetails) <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) ctx
  let order = orderDetails ^. #order
  verifyPayment reqOrder order
  dlBACreds <- getDlBAPCreds org
  fork "confirm" $ do
    createOrderReq <- mkCreateOrderReq order
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_CreateTaskReq") (encodeToText createOrderReq)
    eres <- createOrderAPI dlBACreds dconf createOrderReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_CreateTaskRes") $ show eres
    sendCb order ctx cbApiKey cbUrl eres
  returnAck ctx
  where
    createOrderAPI dlBACreds@DlBAConfig {..} conf@DelhiveryConfig {..} req' = do
      token <- getBearerToken <$> fetchToken dlBACreds conf
      API.createOrder token dlUrl req'

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
        else throwError400 "INVALID_ORDER_AMOUNT"

    sendCb order context cbApiKey cbUrl res =
      case res of
        Right _ -> do
          onConfirmReq <- mkOnConfirmReq context order
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm req") $ encodeToText onConfirmReq
          eres <- L.callAPI cbUrl $ ET.client onConfirmAPI cbApiKey onConfirmReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm res") $ show eres
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onConfirmReq = mkOnConfirmErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm err req") $ encodeToText onConfirmReq
              onConfirmResp <- L.callAPI cbUrl $ ET.client onConfirmAPI cbApiKey onConfirmReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_confirm err res") $ show onConfirmResp
        _ -> pass

fetchToken :: DlBAConfig -> DelhiveryConfig -> Flow Token
fetchToken DlBAConfig {..} DelhiveryConfig {..} = do
  eres <- API.getToken dlTokenUrl (TokenReq dlClientId dlClientSecret "client_credentials")
  case eres of
    Left err -> throwError500 $ show err
    Right tokenRes -> return (tokenRes ^. #access_token)

getQuote :: DlBAConfig -> DelhiveryConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote ba@DlBAConfig {..} conf@DelhiveryConfig {..} quoteReq = do
  token <- getBearerToken <$> fetchToken ba conf
  API.getQuote token dlUrl quoteReq

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack "ACK") Nothing

getBearerToken :: Token -> Token
getBearerToken a = Token (T.pack "Bearer " <> getToken a)
