module API.Confirm where

import API.Confirm.Coerce
import API.Utils
import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import qualified Common.Redis as Redis
import Common.Utils
import Core.Confirm
import Core.OnCancel
import Core.OnConfirm
import qualified Core.OnConfirm as OnConfirm
import Core.OnStatus
import Core.Payment
import ExternalAPI
import MockData.OnConfirm
import Types.App

confirmServer :: BecknReq ConfirmMessage -> MockM AckResponse
confirmServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq

  context' <- buildOnActionContext ON_CONFIRM ctx
  orderId <- generateOrderId
  let eithOrder = buildOnConfirmOrder orderId msg.order
      callbackData = either (Left . textToError) (Right . OnConfirmMessage) eithOrder
  _ <- mockFork $ do
    threadDelaySec 2
    ack <- callBapOnConfirm $ BecknCallbackReq context' callbackData
    mockLog DEBUG $ "got ack" <> show ack
    whenRight eithOrder $ \onConfirmOrder -> do
      Redis.write context' onConfirmOrder
      trackPayment onConfirmOrder.id

  pure Ack

data HandlingWay = Success | FailedPayment | LinkExpired

defineHandlingWay :: Text -> HandlingWay
defineHandlingWay = \case
  "TRIP001_EKM_EMB" -> Success
  "TRIP001_EKM_ABC" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Text -> MockM ()
trackPayment orderId = do
  threadDelaySec 25
  (context, order) <- Redis.readCtxOrderWithException orderId
  let handlingWay = defineHandlingWay order.fulfillment.id
  case handlingWay of
    Success -> transactionOk context order
    FailedPayment -> cancelTransaction Failed context order
    LinkExpired -> cancelTransaction PaymentLinkExpired context order

transactionOk :: Context -> OnConfirm.Order -> MockM ()
transactionOk context order = do
  let onStatusMessage = OnStatusMessage $ coerceOrderStatus $ successfulPayment order
      onStatusReq = BecknCallbackReq (context {action = ON_CANCEL}) (Right onStatusMessage)
  _ <- Redis.editOrder OnConfirm.successfulPayment order.id
  -- but what if we failed to change the state?
  callBapOnStatus onStatusReq

cancelTransaction :: TrStatus -> Context -> OnConfirm.Order -> MockM ()
cancelTransaction trStatus ctx ord = do
  let ctx' = ctx {action = ON_CANCEL}
      onCancelReq = BecknCallbackReq ctx' (Right $ OnCancelMessage $ coerceOrderCancel $ OnConfirm.failedTransaction trStatus ord)
  _ <- Redis.editOrder (OnConfirm.failedTransaction trStatus) ord.id
  -- but what if we failed to change the state?
  callBapOnCancel onCancelReq
