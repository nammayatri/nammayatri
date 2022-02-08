module API.Confirm where

import API.Utils (buildOnActionContext)
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Forkable
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Core.Common.Payment
import qualified Core.Confirm as Confirm
import Core.OnConfirm
import qualified Core.OnStatus as OnStatus
import Environment
import ExternalAPI
import MockData.OnConfirm
import qualified Redis
import Relude hiding (state)
import Utils

confirmServer :: BecknReq Confirm.ConfirmMessage -> MockM AppEnv AckResponse
confirmServer confirmReq@(BecknReq ctx req) = do
  logOutput DEBUG $ "request body: " <> show confirmReq
  context' <- buildOnActionContext ON_CONFIRM ctx
  orderId <- generateOrderId
  let eithOrder = buildOnConfirmOrder orderId req.order
  _ <- fork "call on_confirm" $ do
    waitMilliSec <- asks (.config.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnConfirmCallbackData = OnConfirmMessage <$> first textToError eithOrder
    ack <- callBapOnConfirm $ BecknCallbackReq context' eithOnConfirmCallbackData
    logOutput DEBUG $ "got ack" <> show ack
    whenRight_ eithOrder $ \onConfirmOrder -> do
      Redis.writeOrder context' onConfirmOrder
      trackPayment onConfirmOrder.id

  pure Ack

data HandlingWay = Success | FailedPayment | LinkExpired
  deriving (Show)

defineHandlingWay :: Text -> HandlingWay
defineHandlingWay = \case
  "4" -> Success
  "9" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Text -> MockM AppEnv ()
trackPayment orderId = do
  secondsToWait <- asks (.config.statusWaitTimeSec)
  logOutput INFO $ "waiting " <> show secondsToWait <> " seconds before changing payment status"
  threadDelaySec secondsToWait
  (context, order) <- Redis.readOrder orderId
  item <- fromEitherM InvalidRequest $ validateUnique "item" order.items
  let handlingWay = defineHandlingWay item.id
  logOutput INFO $ "handling item with id = " <> item.id <> " using a handling way: " <> show handlingWay
  case handlingWay of
    Success -> transactionOk context order
    FailedPayment -> cancelTransaction REFUNDED context order -- where is FAILED constructor?
    LinkExpired -> cancelTransaction PAYMENT_LINK_EXPIRED context order

transactionOk :: Context -> Order -> MockM AppEnv ()
transactionOk context order = do
  let onStatusMessage = OnStatus.OnStatusMessage $ successfulPayment order
      onStatusReq = BecknCallbackReq (context {action = ON_STATUS}) (Right onStatusMessage)
  _ <- Redis.editOrder successfulPayment order.id
  -- what if we failed to change the state?
  callBapOnStatus onStatusReq

successfulPayment :: Order -> Order
successfulPayment = changePaymentStatus (Just ACTIVE) CAPTURED PAID

cancelTransaction :: PaymentGatewayTransactionStatus -> Context -> Order -> MockM AppEnv ()
cancelTransaction trStatus context order = do
  let onStatusMessage = OnStatus.OnStatusMessage $ paymentFailed trStatus order
      onStatusReq = BecknCallbackReq context {action = ON_STATUS} $ Right onStatusMessage
  _ <- Redis.editOrder (paymentFailed trStatus) order.id
  callBapOnStatus onStatusReq

-- this function should use on_cancel endpoint, but it is currently absent in the parking-bap

paymentFailed :: PaymentGatewayTransactionStatus -> Order -> Order
paymentFailed trStatus = changePaymentStatus (Just CANCELLED) trStatus NOT_PAID

changePaymentStatus :: Maybe OrderState -> PaymentGatewayTransactionStatus -> PaymentStatus -> Order -> Order
changePaymentStatus orderState trStatus paymStatus order =
  order
    { state = orderState,
      payment =
        order.payment
          { params =
              order.payment.params
                { transaction_status = trStatus
                },
            status = paymStatus
          }
    }
