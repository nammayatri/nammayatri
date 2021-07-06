{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens ((?~))
import Control.Lens.Combinators hiding (Context)
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
import qualified ExternalAPI.Dunzo.Flow as API
import ExternalAPI.Dunzo.Types
import Product.Dunzo.Transform
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Types.Beckn.API.Cancel as CancelAPI
import qualified Types.Beckn.API.Confirm as ConfirmAPI
import qualified Types.Beckn.API.Init as InitAPI
import qualified Types.Beckn.API.Search as SearchAPI
import qualified Types.Beckn.API.Select as SelectAPI
import qualified Types.Beckn.API.Status as StatusAPI
import qualified Types.Beckn.API.Track as TrackAPI
import qualified Types.Beckn.API.Types as API
import qualified Types.Beckn.API.Update as UpdateAPI
import Types.Beckn.Context (Action (..))
import Types.Beckn.DecimalValue (convertDecimalValueToAmount)
import Types.Beckn.Order (Order)
import Types.Common
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Wrapper
import Utils.Common

search ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq SearchAPI.SearchIntent ->
  m AckResponse
search org req = do
  config@DunzoConfig {..} <- asks (.dzConfig)
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req.context) dzBPNwAddress
  bap <- Org.findByBapUrl context.bap_uri >>= fromMaybeM OrgDoesNotExist
  dzBACreds <- getDzBAPCreds bap
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withCallback SEARCH SearchAPI.onSearchAPI context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

select :: MonadFlow m => Org.Organization -> API.BecknReq SelectAPI.SelectedObject -> m AckResponse
select _org _req = throwError $ ActionNotSupported "select"

init ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq InitAPI.InitOrder ->
  m AckResponse
init org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  dzBACreds <- getDzBAPCreds org
  let order = req.message
  validateOrder order

  let caseId = Id req.context.transaction_id
  mbCase <- Storage.findById caseId
  mbCaseValidated <- case mbCase of
    Nothing -> pure Nothing
    Just currCase ->
      if currCase.status == NEW
        then pure $ Just currCase
        else throwError (InvalidRequest "Invalid order status.")

  quoteReq <- mkQuoteReqFromInitOrder order
  withCallback INIT InitAPI.onInitAPI context cbUrl $ do
    onInitMessage <-
      getQuote dzBACreds conf quoteReq
        >>= mkOnInitMessage dzQuotationTTLinMin order
    createOrUpdateCase (getId $ org.id) onInitMessage caseId mbCaseValidated

    return $ InitAPI.InitializedObject onInitMessage
  where
    createOrUpdateCase orgId onInitMessage caseId mbCaseValidated = do
      now <- getCurrentTime
      order <- mkOrderFromInititialized onInitMessage now
      let newCase =
            Case
              { id = caseId,
                name = Nothing,
                description = Nothing,
                shortId = "",
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
                udf1 = Just $ encodeToText order,
                udf2 = Nothing,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      maybe (Storage.create newCase) (const $ Storage.update caseId newCase) mbCaseValidated

    validateOrder order = do
      void $ order.fulfillment.start & fromMaybeM (InvalidRequest "Pickup location not found.")
      void $ order.fulfillment.end & fromMaybeM (InvalidRequest "Drop location not found.")
      void $ getItem order.items
    getItem [item] = pure item
    getItem _ = throwError $ InvalidRequest "Exactly 1 order item expected."

confirm ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq API.OrderObject ->
  m AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req.message.order
  whenJust reqOrder.id \_ -> throwError $ InvalidRequest "Order id must not be presented."
  txnId <-
    reqOrder ^. #payment . #params . _Just . #transaction_id
      & fromMaybeErr "TXN_ID_NOT_PRESENT" Nothing
  let caseId = context.transaction_id
  mbCase <- Storage.findById $ Id caseId
  order <- case mbCase of
    Nothing -> pure reqOrder
    Just case_ -> do
      initOrder <- case_.udf1 >>= decodeFromText & fromMaybeErr "INIT_ORDER_NOT_FOUND" (Just CORE003)
      validateDelayFromInit dzQuotationTTLinMin case_
      verifyPayment reqOrder initOrder
      pure $ initOrder & ((#payment . #params . _Just . #transaction_id) ?~ txnId)
  dzBACreds <- getDzBAPCreds org
  orderId <- generateGUID
  currTime <- getCurrentTime
  taskReq <- mkCreateTaskReq orderId order
  withCallback CONFIRM ConfirmAPI.onConfirmAPI context cbUrl $ do
    taskStatus <- createTaskAPI dzBACreds dconf taskReq
    let uOrder = updateOrder (Just orderId) currTime order taskStatus
    checkAndLogPriceDiff order uOrder
    maybe
      (createCase orderId uOrder taskStatus currTime)
      (\c -> updateCase c orderId uOrder taskStatus)
      mbCase
    return $ API.OrderObject uOrder
  where
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

    createCase orderId order taskStatus now = do
      let caseId = Id req.context.transaction_id
      let case_ =
            Case
              { id = caseId,
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
                parentCaseId = Nothing,
                fromLocationId = "",
                toLocationId = "",
                udf1 = Just $ encodeToText order,
                udf2 = Just $ encodeToText taskStatus,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      Storage.create case_

    updateCase case_ orderId order taskStatus = do
      let caseId = case_.id
      let updatedCase =
            case_
              { shortId = ShortId orderId,
                Case.status = CONFIRMED,
                udf1 = Just $ encodeToText order,
                udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

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

    validateDelayFromInit dzQuotationTTLinMin case_ = do
      now <- getCurrentTime
      let orderCreatedAt = case_.createdAt
      let thresholdTime = addUTCTime (fromInteger (dzQuotationTTLinMin * 60)) orderCreatedAt
      when (thresholdTime < now) $
        throwError (InvalidRequest "Took too long to confirm.")

track ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq TrackAPI.TrackInfo ->
  m AckResponse
track org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.message.order_id
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId case_.shortId
  dzBACreds <- getDzBAPCreds org
  withCallback TRACK TrackAPI.onTrackAPI context cbUrl $
    getStatus dzBACreds conf (TaskId taskId) <&> mkOnTrackMessage . (.tracking_url)

status ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq StatusAPI.OrderId ->
  m AckResponse
status org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let orderId = context.transaction_id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId case_.shortId
  order <-
    case_.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallback STATUS StatusAPI.onStatusAPI context cbUrl $ do
    taskStatus <- getStatus dzBACreds conf (TaskId taskId)
    onStatusMessage <- mkOnStatusMessage order taskStatus
    let updatedOrder = onStatusMessage.order
    updateCase (case_.id) updatedOrder taskStatus case_
    return onStatusMessage
  where
    updateCase caseId order taskStatus case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText order, udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

cancel ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq CancelAPI.CancellationInfo ->
  m AckResponse
cancel org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id context.transaction_id) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId $ case_.shortId
  order <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallback CANCEL CancelAPI.onCancelAPI context cbUrl $ do
    callCancelAPI dzBACreds conf (TaskId taskId)
    let updatedOrder = cancelOrder order
    updateCase case_.id updatedOrder case_
    return $ API.OrderObject updatedOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: DBFlow m r => Id Case -> Order -> Case -> m ()
    updateCase caseId order case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText order}
      Storage.update caseId updatedCase

update :: MonadFlow m => Org.Organization -> API.BecknReq UpdateAPI.UpdateInfo -> m AckResponse
update _org _req = throwError $ ActionNotSupported "update"

-- Helpers
getQuote ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  QuoteReq ->
  m QuoteRes
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  TaskId ->
  m TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  m Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

withCallback :: WithBecknCallbackMig api callback_success m
withCallback = withBecknCallbackMig (Just HttpSig.signatureAuthManagerKey)
