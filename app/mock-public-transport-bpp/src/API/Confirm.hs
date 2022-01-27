module API.Confirm where

import API.Confirm.Coerce
import API.Utils
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Confirm
import Core.OnCancel
import Core.OnConfirm
import qualified Core.OnConfirm as OnConfirm
import Core.OnStatus
import Core.Payment
import Environment
import ExternalAPI
import MockData.OnConfirm
import qualified Redis
import Relude

confirmServer :: BecknReq ConfirmMessage -> MockM AppEnv AckResponse
confirmServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq

  context' <- buildOnActionContext ON_CONFIRM ctx
  orderId <- generateOrderId
  let eithOrder = buildOnConfirmOrder orderId msg.order
      callbackData = either (Left . textToError) (Right . OnConfirmMessage) eithOrder
  _ <- mockFork $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    callBapOnConfirm $ BecknCallbackReq context' callbackData
    whenRight_ eithOrder $ \onConfirmOrder -> do
      Redis.writeOrder context' onConfirmOrder
      trackPayment onConfirmOrder.id

  pure Ack

data HandlingWay = Success | FailedPayment | LinkExpired
  deriving (Show)

defineHandlingWay :: Text -> HandlingWay
defineHandlingWay = \case
  "TRIP001_EKM_EMB" -> Success
  "TRIP001_EKM_ABC" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Text -> MockM AppEnv ()
trackPayment orderId = do
  secondsToWait <- asks (.statusWaitTimeSec)
  mockLog INFO $ "waiting " <> show secondsToWait <> " seconds before changing payment status"
  threadDelaySec secondsToWait
  (context, order) <- Redis.readOrder orderId
  let handlingWay = defineHandlingWay order.fulfillment.id
  mockLog INFO $ "handling orderId=" <> orderId <> " with handlingWay=" <> show handlingWay
  case handlingWay of
    Success -> transactionOk context order
    FailedPayment -> cancelTransaction Failed context order
    LinkExpired -> cancelTransaction PaymentLinkExpired context order

transactionOk :: Context -> OnConfirm.Order -> MockM AppEnv ()
transactionOk context order = do
  let onStatusMessage = OnStatusMessage $ coerceOrderStatus $ successfulPayment order
      onStatusReq = BecknCallbackReq (context {action = ON_CANCEL}) (Right onStatusMessage)
  mockLog INFO $ "editing order with orderId=" <> order.id <> "; successful payment"
  _ <- Redis.editOrder OnConfirm.successfulPayment order.id
  -- but what if we failed to change the state?
  callBapOnStatus onStatusReq

cancelTransaction :: TrStatus -> Context -> OnConfirm.Order -> MockM AppEnv ()
cancelTransaction trStatus ctx order = do
  let ctx' = ctx {action = ON_CANCEL}
      onCancelReq = BecknCallbackReq ctx' (Right $ OnCancelMessage $ coerceOrderCancel $ OnConfirm.failedTransaction trStatus order)
  mockLog INFO $ "editing order with orderId=" <> order.id <> "; payment failed"
  _ <- Redis.editOrder (OnConfirm.failedTransaction trStatus) order.id
  -- but what if we failed to change the state?
  callBapOnCancel onCancelReq
