module API.Beckn.FRFSSeller.Handler
  ( sellerAck,
    dedupeKey,
    acceptOnce,
    claimOnce,
  )
where

import qualified BecknV2.FRFS.Types as Spec
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common

sellerAck :: Spec.AckResponse
sellerAck =
  Spec.AckResponse
    { ackResponseError = Nothing,
      ackResponseMessage =
        Spec.AckMessage
          { ackMessageAck = Spec.Ack {ackStatus = Just "ACK", ackTags = Nothing}
          }
    }

dedupeKey :: Text -> Text -> Text -> Text -> Text
dedupeKey operator action txnId msgId = "frfsSeller:" <> operator <> ":" <> action <> ":" <> txnId <> ":" <> msgId

dedupeTtlSeconds :: Redis.ExpirationTime
dedupeTtlSeconds = 60

acceptOnce :: Text -> Text -> Maybe Text -> Maybe Text -> Flow () -> Flow Spec.AckResponse
acceptOnce operator action mbTransactionId mbMessageId work = do
  claimOnce operator action mbTransactionId mbMessageId work
  pure sellerAck

claimOnce :: Text -> Text -> Maybe Text -> Maybe Text -> Flow () -> Flow ()
claimOnce operator action mbTransactionId mbMessageId work = do
  transactionId <- mbTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- mbMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  withTransactionIdLogTag' transactionId $ do
    isFirst <-
      try @_ @SomeException
        (Redis.withCrossAppRedis $ Redis.setNxExpire (dedupeKey operator action transactionId messageId) dedupeTtlSeconds True)
        >>= \case
          Right claimed -> pure claimed
          Left err -> do
            logWarning $ "FRFS seller " <> action <> " dedupe unavailable, processing anyway: " <> show err
            pure True
    if isFirst
      then do
        logInfo $ "FRFS seller " <> action <> " accepted: msg=" <> messageId
        fork ("FRFS seller " <> action <> " processing") work
      else logInfo $ "FRFS seller " <> action <> " duplicate ignored: msg=" <> messageId
