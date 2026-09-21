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
          -- Fail closed. Processing anyway would let a buyer's retry issue a second
          -- CDAC ticket and a second settlement row, and the claim is the only thing
          -- standing between a retry and duplicate paid work. Nothing is lost by
          -- refusing: confirm takes its own Redis lock (@Confirm.hs:160@), so these
          -- flows cannot complete correctly without Redis regardless -- this just
          -- fails cleanly and lets the BAP retry instead of double-issuing.
          Left err -> do
            logError $ "FRFS seller " <> action <> " dedupe unavailable, refusing: " <> show err
            throwError . InternalError $ "FRFS seller " <> action <> " dedupe unavailable"
    if isFirst
      then do
        logInfo $ "FRFS seller " <> action <> " accepted: msg=" <> messageId
        -- The claim must not outlive failed work: we have already ACKed, so if `work` dies
        -- before sending its callback the BAP's retry (same message_id, inside the TTL) would
        -- be swallowed as a duplicate and the order would strand. Give the retry its slot back.
        fork ("FRFS seller " <> action <> " processing") $
          work `catchAny` \err -> do
            logError $ "FRFS seller " <> action <> " failed, releasing dedupe claim: msg=" <> messageId <> " err=" <> show err
            void . try @_ @SomeException . Redis.withCrossAppRedis $ Redis.del (dedupeKey operator action transactionId messageId)
      else logInfo $ "FRFS seller " <> action <> " duplicate ignored: msg=" <> messageId
