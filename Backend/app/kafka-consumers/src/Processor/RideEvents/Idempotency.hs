-- | Per-handler idempotency guard for ride-event processors.
-- Each handler attempts to claim the (handler, rideId) key via SETNX with a TTL.
-- If the key already exists, the handler is skipped — protecting against
-- re-delivery from PEL claim or DLQ replay.
module Processor.RideEvents.Idempotency
  ( withIdempotency,
    idempotencyTTLSeconds,
  )
where

import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (logInfo)

-- 24h far exceeds any reasonable Redis Stream max-delivery-delay (PEL idle, claim, retry).
idempotencyTTLSeconds :: Int
idempotencyTTLSeconds = 86400

-- Atomic SETNX-with-TTL: returns True only on first call within the TTL.
withIdempotency :: Text -> Text -> Flow () -> Flow ()
withIdempotency handlerName rideId action = do
  let key = "ride:processed:" <> handlerName <> ":" <> rideId
  fresh <- Hedis.setNxExpire key idempotencyTTLSeconds ("1" :: Text)
  if fresh
    then action
    else logInfo $ "ride-events.idempotency-skip handler=" <> handlerName <> " rideId=" <> rideId
