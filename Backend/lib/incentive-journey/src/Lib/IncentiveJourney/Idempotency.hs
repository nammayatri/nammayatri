{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Idempotency
  ( withJourneyEvalIdempotency,
    journeyEvalIdempotencyTTLSeconds,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow, MonadFlow, logInfo)
import Lib.IncentiveJourney.Types.Actor (JourneyActor, actorCachePrefix)

-- | 24h exceeds typical Beckn / stream retry windows.
journeyEvalIdempotencyTTLSeconds :: Int
journeyEvalIdempotencyTTLSeconds = 86400

-- | SETNX-with-TTL guard so retries cannot re-apply deltas to InProgress milestones.
withJourneyEvalIdempotency ::
  (MonadFlow m, CacheFlow m r) =>
  JourneyActor ->
  Text ->
  m () ->
  m ()
withJourneyEvalIdempotency actor rideId action = do
  let key = actorCachePrefix actor <> ":ride:processed:incentiveJourney:" <> rideId
  fresh <- Hedis.setNxExpire key journeyEvalIdempotencyTTLSeconds ("1" :: Text)
  if fresh
    then action
    else logInfo $ "incentive-journey.idempotency-skip actor=" <> show actor <> " rideId=" <> rideId
