module Lib.Payment.Payout.StatusCheck
  ( PayoutStatusCheckJobData (..),
    PayoutStatusCheckConfig (..),
    Handle (..),
    isPayoutOrderSuccess,
    isPayoutStatusFailed,
    isTerminalPayoutOrderStatus,
    runPayoutStatusCheckJob,
    schedulePayoutStatusCheck,
  )
where

import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.PayoutOrder (PayoutOrder)
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Lib.Scheduler.Types (ExecutionResult (..))

data PayoutStatusCheckJobData = PayoutStatusCheckJobData
  { payoutOrderId :: Text,
    attempt :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data PayoutStatusCheckConfig = PayoutStatusCheckConfig
  { checkInterval :: NominalDiffTime,
    maxAttempts :: Int
  }

data Handle m = Handle
  { getConfig :: PayoutOrder -> m PayoutStatusCheckConfig,
    refreshWithSettlement :: PayoutOrder -> m PayoutOrder,
    scheduleNextCheck :: PayoutOrder -> PayoutStatusCheckJobData -> NominalDiffTime -> m ()
  }

isPayoutOrderSuccess :: IPayout.PayoutOrderStatus -> Bool
isPayoutOrderSuccess status = status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]

isPayoutStatusFailed :: IPayout.PayoutOrderStatus -> Bool
isPayoutStatusFailed status = status `elem` [Payout.FAILURE, Payout.FULFILLMENTS_FAILURE, Payout.FULFILLMENTS_CANCELLED]

isTerminalPayoutOrderStatus :: IPayout.PayoutOrderStatus -> Bool
isTerminalPayoutOrderStatus status = isPayoutOrderSuccess status || isPayoutStatusFailed status || status `elem` [Payout.ERROR, Payout.CANCELLED]

runPayoutStatusCheckJob :: (BeamFlow m r) => Handle m -> PayoutStatusCheckJobData -> m ExecutionResult
runPayoutStatusCheckJob h jobData = withLogTag ("PayoutStatusCheck-" <> jobData.payoutOrderId) $ do
  mbOrder <- QPayoutOrder.findByOrderId jobData.payoutOrderId
  case mbOrder of
    Nothing -> pure $ Terminate ("Payout order not found: " <> jobData.payoutOrderId)
    Just order
      | isTerminalPayoutOrderStatus order.status -> do
        logInfo $ "Payout order already terminal (" <> show order.status <> "), nothing to do"
        pure Complete
      | otherwise -> do
        config <- h.getConfig order
        refreshed <-
          withTryCatch "refreshPayoutOrderWithSettlement" (h.refreshWithSettlement order) <&> \case
            Right refreshedOrder -> refreshedOrder
            Left _ -> order
        QPayoutOrder.updateLastCheckedOn [order.orderId]
        if isTerminalPayoutOrderStatus refreshed.status
          then do
            logInfo $ "Payout order reached terminal status " <> show refreshed.status <> " after " <> show (jobData.attempt + 1) <> " checks"
            pure Complete
          else do
            let nextAttempt = jobData.attempt + 1
            if nextAttempt >= config.maxAttempts
              then do
                logError $ "Payout order still " <> show refreshed.status <> " after " <> show nextAttempt <> " checks, giving up"
                pure $ Terminate "Max payout status check attempts reached"
              else do
                h.scheduleNextCheck refreshed jobData {attempt = nextAttempt} config.checkInterval
                pure Complete

schedulePayoutStatusCheck ::
  (MonadFlow m) =>
  (PayoutOrder -> PayoutStatusCheckJobData -> NominalDiffTime -> m ()) ->
  PayoutStatusCheckConfig ->
  PayoutOrder ->
  m ()
schedulePayoutStatusCheck schedule config order
  | isTerminalPayoutOrderStatus order.status = logInfo $ "Payout order " <> order.orderId <> " already terminal (" <> show order.status <> "), skipping status check job"
  | otherwise = schedule order (PayoutStatusCheckJobData {payoutOrderId = order.orderId, attempt = 0}) config.checkInterval
