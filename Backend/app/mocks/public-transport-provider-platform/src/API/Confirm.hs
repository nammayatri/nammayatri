{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Confirm where

import API.Utils
import "public-transport-rider-platform" Beckn.Spec.Common
import qualified "public-transport-rider-platform" Beckn.Spec.Confirm as Confirm
import "public-transport-rider-platform" Beckn.Spec.OnCancel
import "public-transport-rider-platform" Beckn.Spec.OnConfirm
import "public-transport-rider-platform" Beckn.Spec.OnStatus
import Environment
import ExternalAPI
import Kernel.Mock.App
import Kernel.Mock.Utils
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Time
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
  let ctx' = ctx {action = ON_CANCEL} :: Context
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
