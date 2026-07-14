-- | The engine's runtime environment: all the effect handles bundled with the
-- static config. The pure engine takes a @BotEnv m@ and never reaches for any
-- effect outside it, so the golden suite injects a mock env and rider-app
-- injects a 'Flow' env with the same engine code.
module WhatsappBot.Env
  ( BotConfig (..),
    BotEnv (..),
  )
where

import Kernel.Prelude
import WhatsappBot.Handles
import WhatsappBot.Types (MerchantCtx)

-- | Static per-conversation config: the allowlist, the resolved merchant, and
-- the poll-loop constants (TS parity, @engine.ts@/@config.ts@). Held separate
-- from the handles so tests can vary it without touching the effect layer.
data BotConfig = BotConfig
  { -- | Normalized 10-digit allowlist; empty = open to all (@config.ts:112-118@;
    -- default @["9361176218"]@).
    allowedPhones :: [Text],
    -- | The merchant resolved for this session (by phone_number_id upstream).
    merchant :: MerchantCtx,
    -- | Flexi quote poll: attempts × interval (@engine.ts:1074-1078@; 10 × 2000ms).
    flexiQuotePollAttempts :: Int,
    flexiQuotePollIntervalMs :: Int,
    -- | Regular estimate poll (@engine.ts:825-829@; 6 × 2000ms).
    regularEstimatePollAttempts :: Int,
    regularEstimatePollIntervalMs :: Int,
    -- | Driver-assignment poll (@engine.ts:890-906,1123-1151@; 90 × 2000ms,
    -- progress notify every 15).
    driverPollAttempts :: Int,
    driverPollIntervalMs :: Int,
    driverPollNotifyEvery :: Int
  }

-- | Everything the engine needs, in one record. @m@ is 'IO' (over mock IORefs)
-- in the golden suite and rider-app's @Flow@ in production.
data BotEnv m = BotEnv
  { backend :: BackendHandle m,
    sender :: WaSender m,
    sessions :: SessionStore m,
    persons :: PersonStore m,
    registry :: RideRegistry m,
    clock :: Clock m,
    cfg :: BotConfig
  }
