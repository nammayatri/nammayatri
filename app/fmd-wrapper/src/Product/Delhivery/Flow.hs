{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Flow where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.Ack (AckResponse (..), Status (..), ack)
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.API.Confirm as API
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Types.FMD.Order
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified Data.Text as T
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

search :: Org.Organization -> API.SearchReq -> Flow API.SearchRes
search org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  bapUrl <- context ^. #_bap_uri & fromMaybeM (InvalidRequest "You should pass bap uri.")
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM OrgDoesNotExist
  dlBACreds <- getDlBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dlBACreds config quoteReq
    sendCb context eres
  returnAck context
  where
    sendCb context res = do
      cbUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          logTagInfo (req ^. #context . #_transaction_id <> "_on_search req") $ encodeToText onSearchReq
          onSearchResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSearchAPI onSearchReq
          logTagInfo (req ^. #context . #_transaction_id <> "_on_search res") $ show onSearchResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSearchErrReq = mkOnSearchErrReq context err
              logTagInfo (req ^. #context . #_transaction_id <> "_on_search err req") $ encodeToText onSearchErrReq
              onSearchResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSearchAPI onSearchErrReq
              logTagInfo (req ^. #context . #_transaction_id <> "_on_search err res") $ show onSearchResp
        _ -> pass

select :: Org.Organization -> API.SelectReq -> Flow API.SelectRes
select org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  --  validateOrderRequest $ req ^. #message . #order
  cbUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  dlBACreds <- getDlBAPCreds org
  fork "Select" $ do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote dlBACreds config quoteReq
    logTagInfo (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCallback context cbUrl eres
  returnAck context
  where
    sendCallback context cbUrl res =
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
          logTagInfo (req ^. #context . #_transaction_id <> "_on_select req") $ encodeToText onSelectReq
          onSelectResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSelectAPI onSelectReq
          logTagInfo (req ^. #context . #_transaction_id <> "_on_select res") $ show onSelectResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSelectReq = mkOnSelectErrReq context err
              logTagInfo (req ^. #context . #_transaction_id <> "_on_select err req") $ encodeToText onSelectReq
              onSelectResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onSelectAPI onSelectReq
              logTagInfo (req ^. #context . #_transaction_id <> "_on_select err res") $ show onSelectResp
        _ -> pass

init :: Org.Organization -> API.InitReq -> Flow API.InitRes
init org req = do
  conf@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  quote <- req ^. (#message . #order . #_quotation) & fromMaybe400Log "INVALID_QUOTATION" (Just CORE003) context
  let quoteId = quote ^. #_id
  payeeDetails <- dlPayee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybe400Log "INVALID_QUOTATION_ID" (Just CORE003) context
  dlBACreds <- getDlBAPCreds org
  fork "init" $ do
    quoteReq <- mkQuoteReqFromSelect $ API.SelectReq context (API.SelectOrder (orderDetails ^. #order))
    eres <- getQuote dlBACreds conf quoteReq
    logTagInfo (req ^. #context . #_transaction_id <> "_QuoteRes") $ show eres
    sendCb orderDetails context cbUrl payeeDetails quoteId eres
  returnAck context
  where
    sendCb orderDetails context cbUrl payeeDetails quoteId (Right res) = do
      -- quoteId will be used as orderId
      onInitMessage <-
        mkOnInitMessage
          quoteId
          (orderDetails ^. #order)
          payeeDetails
          req
          res
      let onInitReq = mkOnInitReq context onInitMessage
      createCaseIfNotPresent (getId $ org ^. #_id) (onInitMessage ^. #order) (orderDetails ^. #quote)
      logTagInfo (req ^. #context . #_transaction_id <> "_on_init req") $ encodeToText onInitReq
      onInitResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onInitAPI onInitReq
      logTagInfo (req ^. #context . #_transaction_id <> "_on_init res") $ show onInitResp
      return ()
    sendCb _ context cbUrl _ _ (Left (FailureResponse _ (Response _ _ _ body))) =
      case decode body of
        Just err -> do
          let onInitReq = mkOnInitErrReq context err
          logTagInfo (req ^. #context . #_transaction_id <> "_on_init err req") $ encodeToText onInitReq
          onInitResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onInitAPI onInitReq
          logTagInfo (req ^. #context . #_transaction_id <> "_on_init err res") $ show onInitResp
          return ()
        Nothing -> return ()
    sendCb _ _ _ _ _ _ = return ()

    createCaseIfNotPresent orgId order quote = do
      now <- getCurrentTime
      let caseId = Id $ fromJust $ order ^. #_id
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

confirm :: Org.Organization -> API.ConfirmReq -> Flow API.ConfirmRes
confirm org req = do
  dconf@DelhiveryConfig {..} <- dlConfig <$> ask
  let ctx = updateBppUri (req ^. #context) dlBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybe400Log "INVALID_ORDER_ID" (Just CORE003) ctx $ reqOrder ^. #_id
  case_ <- Storage.findById (Id orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) ctx
  (orderDetails :: OrderDetails) <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) ctx
  let order = orderDetails ^. #order
  verifyPayment reqOrder order
  dlBACreds <- getDlBAPCreds org
  fork "confirm" $ do
    createOrderReq <- mkCreateOrderReq order
    logTagInfo (req ^. #context . #_transaction_id <> "_CreateTaskReq") (encodeToText createOrderReq)
    eres <- createOrderAPI dlBACreds dconf createOrderReq
    logTagInfo (req ^. #context . #_transaction_id <> "_CreateTaskRes") $ show eres
    sendCb order ctx cbUrl eres
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
        else throwError $ InvalidRequest "Invalid order amount."

    sendCb order context cbUrl res =
      case res of
        Right _ -> do
          onConfirmReq <- mkOnConfirmReq context order
          logTagInfo (req ^. #context . #_transaction_id <> "_on_confirm req") $ encodeToText onConfirmReq
          eres <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onConfirmAPI onConfirmReq
          logTagInfo (req ^. #context . #_transaction_id <> "_on_confirm res") $ show eres
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onConfirmReq = mkOnConfirmErrReq context err
              logTagInfo (req ^. #context . #_transaction_id <> "_on_confirm err req") $ encodeToText onConfirmReq
              onConfirmResp <- callAPI' (Just HttpSig.signatureAuthManagerKey) cbUrl $ ET.client API.onConfirmAPI onConfirmReq
              logTagInfo (req ^. #context . #_transaction_id <> "_on_confirm err res") $ show onConfirmResp
        _ -> pass

fetchToken :: DlBAConfig -> DelhiveryConfig -> Flow Token
fetchToken DlBAConfig {..} DelhiveryConfig {..} =
  API.getToken dlTokenUrl (TokenReq dlClientId dlClientSecret "client_credentials")
    >>= fromEitherM (InternalError . show)
    <&> (^. #access_token)

getQuote :: DlBAConfig -> DelhiveryConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote ba@DlBAConfig {..} conf@DelhiveryConfig {..} quoteReq = do
  token <- getBearerToken <$> fetchToken ba conf
  API.getQuote token dlUrl quoteReq

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack ACK) Nothing

getBearerToken :: Token -> Token
getBearerToken a = Token (T.pack "Bearer " <> getToken a)
