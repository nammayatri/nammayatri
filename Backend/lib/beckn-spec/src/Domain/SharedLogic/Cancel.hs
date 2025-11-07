module Domain.SharedLogic.Cancel where

import qualified Data.HashMap.Strict as HM
import Domain.Tools.Error
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common

tryCancellationLock ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasKafkaProducer r
  ) =>
  Text ->
  m a ->
  m a
tryCancellationLock transactionId action = do
  isLockAcquired <- Redis.withCrossAppRedis $ Redis.tryLockRedis (mkCancellationLockKey transactionId) 30
  unless isLockAcquired $ throwError ActiveCancellationOngoing
  exep <- withTryCatch "action:cancelTransaction" action
  case exep of
    Left e -> do
      releaseCancellationLock transactionId
      someExceptionToAPIErrorThrow e
    Right a -> pure a
  where
    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc

releaseCancellationLock :: (CacheFlow m r, EsqDBFlow m r) => Text -> m ()
releaseCancellationLock transactionId = do
  Redis.withCrossAppRedis $ Redis.unlockRedis (mkCancellationLockKey transactionId)

mkCancellationLockKey :: Text -> Text
mkCancellationLockKey transactionId = "isBookingCancellable:" <> transactionId
