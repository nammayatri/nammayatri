module API.Confirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Core.Confirm (ConfirmMessage (..))
import Core.OnCancel
import Core.OnConfirm (OnConfirmMessage (..))
import qualified Core.OnConfirm as OnConfirm
import Core.Payment
import ExternalAPI
import MockData.OnConfirm
import MockData.OnStatus
import Redis
import Types.App
import Utils

confirmServer :: BecknReq ConfirmMessage -> MockM AckResponse
confirmServer confirmReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show confirmReq

  context' <- buildOnConfirmContext ctx
  orderId <- liftIO generateOrderId
  let eithOrder = buildOnConfirmOrder orderId msg.order
      callbackData = either (Left . textToError) (Right . OnConfirmMessage) eithOrder
  _ <- mockFork $ do
    liftIO $ threadDelaySec 2
    let bapUri = ctx.bap_uri
    ack <- callBapOnConfirmS bapUri $ BecknCallbackReq context' callbackData
    mockLog DEBUG $ "got ack" <> show ack
    whenRight eithOrder $ trackPayment context'

  pure Ack

data HandlingWay = Success | FailedPayment | LinkExpired

defineHandlingWay :: Text -> HandlingWay
defineHandlingWay = \case
  "TRIP001_EKM_EMB" -> Success
  "TRIP001_EKM_ABC" -> LinkExpired
  _ -> FailedPayment

trackPayment :: Context -> OnConfirm.Order -> MockM ()
trackPayment ctx ord = do
  Redis.write ctx ord
  liftIO $ threadDelaySec 60
  let handlingWay = defineHandlingWay ord.fulfillment.id
  case handlingWay of
    Success -> do
      let onStatusMessage = OnStatusMessage $ successfulPayment ord
          onStatusReq = BecknCallbackReq ctx (Right onStatusMessage)
      _ <- editOrder OnConfirm.successfulPayment ord.id
      -- but what if we failed to change the state?
      callBapOnStatus onStatusReq
    FailedPayment -> cancelTransaction Failed ctx ord
    LinkExpired -> cancelTransaction PaymentLinkExpired ctx ord

cancelTransaction :: TrStatus -> Context -> OnConfirm.Order -> MockM ()
cancelTransaction trStatus ctx ord = do
  let onCancelReq = BecknCallbackReq ctx (Right $ OnCancelMessage $ coerceOrder $ OnConfirm.failedTransaction trStatus ord)
  _ <- editOrder (OnConfirm.failedTransaction trStatus) ord.id
  callBapOnCancel onCancelReq

buildOnConfirmContext :: Context -> MockM Context
buildOnConfirmContext ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = ON_CONFIRM,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
