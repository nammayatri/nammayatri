{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Flow where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Callback (WithBecknCallback, withBecknCallback)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified ExternalAPI.Delhivery.Flow as API
import ExternalAPI.Delhivery.Types
import Product.Delhivery.Transform
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import qualified Types.Beckn.API.Confirm as API
import qualified Types.Beckn.API.Init as API
import qualified Types.Beckn.API.Search as API
import qualified Types.Beckn.API.Select as API
import Types.Beckn.FmdOrder
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common

search :: Org.Organization -> API.SearchReq -> Flow API.SearchRes
search org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req.context) dlBPNwAddress
  bapUrl <- context.bap_uri & fromMaybeM (InvalidRequest "You should pass bap uri.")
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM OrgDoesNotExist
  dlBACreds <- getDlBAPCreds bap
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withCallback "search" API.onSearchAPI context cbUrl $
    getQuote dlBACreds config quoteReq
      >>= mkOnSearchServices

select :: Org.Organization -> API.SelectReq -> Flow API.SelectRes
select org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req.context) dlBPNwAddress
  --  validateOrderRequest $ req.message.order
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  dlBACreds <- getDlBAPCreds org
  withCallback "select" API.onSelectAPI context cbUrl $ do
    let reqOrder = req.message.order
    onSelectMessage <-
      mkQuoteReqFromSelect req
        >>= getQuote dlBACreds config
        >>= mkOnSelectOrder reqOrder
    let order = onSelectMessage.order
    -- onSelectMessage has quotation
    let quote = fromJust $ onSelectMessage.order.quotation
    let quoteId = quote.id
    let orderDetails = OrderDetails order quote
    Storage.storeQuote quoteId orderDetails
    return onSelectMessage

init :: Org.Organization -> API.InitReq -> Flow API.InitRes
init org req = do
  conf@DelhiveryConfig {..} <- dlConfig <$> ask
  let context = updateBppUri (req.context) dlBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  quote <- req.message.order.quotation & fromMaybeErr "INVALID_QUOTATION" (Just CORE003)
  let quoteId = quote.id
  payeeDetails <- dlPayee & decodeFromText & fromMaybeM (InternalError "Decode error.")
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybeErr "INVALID_QUOTATION_ID" (Just CORE003)
  dlBACreds <- getDlBAPCreds org
  withCallback "init" API.onInitAPI context cbUrl $ do
    onInitMessage <-
      mkQuoteReqFromSelect (API.SelectReq context (API.SelectOrder (orderDetails.order)))
        >>= getQuote dlBACreds conf
        >>= mkOnInitMessage quoteId (orderDetails.order) payeeDetails req
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
                provider = Just "Delhivery",
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
  dconf@DelhiveryConfig {..} <- dlConfig <$> ask
  let ctx = updateBppUri (req.context) dlBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req.message.order
  orderId <- fromMaybeErr "INVALID_ORDER_ID" (Just CORE003) $ reqOrder.id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  (orderDetails :: OrderDetails) <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let order = orderDetails.order
  verifyPayment reqOrder order
  dlBACreds <- getDlBAPCreds org
  withCallback "confirm" API.onConfirmAPI ctx cbUrl $ do
    CreateOrderRes {} <-
      mkCreateOrderReq order
        >>= createOrderAPI dlBACreds dconf
    return $ API.ConfirmResMessage order
  where
    createOrderAPI dlBACreds@DlBAConfig {..} conf@DelhiveryConfig {..} req' = do
      token <- getBearerToken <$> fetchToken dlBACreds conf
      API.createOrder token dlUrl req'

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
        else throwError $ InvalidRequest "Invalid order amount."

fetchToken :: DlBAConfig -> DelhiveryConfig -> Flow Token
fetchToken DlBAConfig {..} DelhiveryConfig {..} =
  API.getToken dlTokenUrl (TokenReq dlClientId dlClientSecret "client_credentials")
    <&> (.access_token)

getQuote :: DlBAConfig -> DelhiveryConfig -> QuoteReq -> Flow QuoteRes
getQuote ba@DlBAConfig {..} conf@DelhiveryConfig {..} quoteReq = do
  token <- getBearerToken <$> fetchToken ba conf
  API.getQuote token dlUrl quoteReq

getBearerToken :: Token -> Token
getBearerToken a = Token (T.pack "Bearer " <> getToken a)

withCallback :: WithBecknCallback api callback_success r
withCallback = withBecknCallback (Just HttpSig.signatureAuthManagerKey)
