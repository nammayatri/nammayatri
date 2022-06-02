module API.Confirm where

import API.Utils
import Beckn.Mock.App
import Beckn.Mock.Utils
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Time
import "public-transport-bap" Core.Spec.Common
import qualified "public-transport-bap" Core.Spec.Confirm as Confirm
import "public-transport-bap" Core.Spec.OnCancel
import "public-transport-bap" Core.Spec.OnConfirm
import "public-transport-bap" Core.Spec.OnStatus
import Environment
import ExternalAPI
import MockData.OnConfirm
import qualified Redis
import Relude.Monad.Either

confirmServer :: BecknReq Confirm.ConfirmMessage -> MockM AppEnv AckResponse
confirmServer confirmReq@(BecknReq ctx msg) = do
  logOutput INFO $ "got confirm request: " <> show confirmReq

  context' <- buildOnActionContext ON_CONFIRM ctx
  orderId <- generateOrderId
  let eithOrder = makeOnConfirmOrder orderId msg.order
      callbackData = either (Left . textToError) (Right . OnConfirmMessage) eithOrder
  _ <- fork "call on_confirm" $ do
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
  "RouteCode-EKM-ABC" -> Success
  "RouteCode-EKM-EMB" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Text -> MockM AppEnv ()
trackPayment orderId = do
  secondsToWait <- asks (.statusWaitTimeSec)
  logOutput INFO $ "waiting " <> show secondsToWait <> " seconds before changing payment status"
  threadDelaySec secondsToWait
  (context, order) <- Redis.readOrder orderId
  let handlingWay = maybe FailedPayment (defineHandlingWay . (.route_code)) $ listToMaybe order.items
  logOutput INFO $ "handling orderId=" <> orderId <> " with handlingWay=" <> show handlingWay
  case handlingWay of
    Success -> transactionOk context order
    FailedPayment -> cancelTransaction FAILED context order
    LinkExpired -> cancelTransaction PAYMENT_LINK_EXPIRED context order

transactionOk :: Context -> Order -> MockM AppEnv ()
transactionOk context order = do
  let onStatusMessage = OnStatusMessage $ successfulPayment order
      onStatusReq = BecknCallbackReq (context {action = ON_STATUS}) (Right onStatusMessage)
  logOutput INFO $ "editing order with orderId=" <> order.id <> "; successful payment"
  _ <- Redis.editOrder successfulPayment order.id
  -- but what if we failed to change the state?
  callBapOnStatus onStatusReq

cancelTransaction :: TrStatus -> Context -> Order -> MockM AppEnv ()
cancelTransaction trStatus ctx order = do
  let ctx' = ctx {action = ON_CANCEL}
      onCancelReq = BecknCallbackReq ctx' (Right $ OnCancelMessage $ failedTransaction trStatus order)
  logOutput INFO $ "editing order with orderId=" <> order.id <> "; payment failed"
  _ <- Redis.editOrder (failedTransaction trStatus) order.id
  -- but what if we failed to change the state?
  callBapOnCancel onCancelReq

------------------------------------------

changePaymentState :: Status -> TrStatus -> Order -> Order
changePaymentState st trStatus ord =
  ord{payment =
        ord.payment
          { status = st,
            params = (ord.payment.params :: Params) {transaction_status = trStatus}
          }
     }

successfulPayment :: Order -> Order
successfulPayment = changePaymentState PAID CAPTURED . changeBookingState ACTIVE

changeBookingState :: State -> Order -> Order
changeBookingState state' ord = ord {state = state'}

failedTransaction :: TrStatus -> Order -> Order
failedTransaction st = changePaymentState NOT_PAID st . changeBookingState CANCELLED
