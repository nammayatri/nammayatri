module SharedLogic.Allocator.Jobs.Settlement.Lua where

import qualified Data.ByteString.Char8 as B
import Kernel.Prelude

-- | Atomically increments a counter key ONLY if it already exists.
--
--   Prevents the race condition where Redis restarts and the key is lost:
--     GET → Just 500 → (Redis restart, key lost) → INCR → creates at 0 → returns 1
--
--   With this script the INCR never fires on a missing key; the caller
--   falls back to DB-seeded initialisation instead.
--
--   KEYS:
--     [ counterKey ]
--
--   ARGV: (none)
--
--   Returns:
--     Integer (new counter value) → key existed, INCR succeeded
--     nil                         → key missing, caller must re-seed from DB
incrIfExistsScript :: B.ByteString
incrIfExistsScript =
  B.pack $
    unlines
      [ "if redis.call('EXISTS', KEYS[1]) == 1 then",
        "  return redis.call('INCR', KEYS[1])",
        "else",
        "  return nil",
        "end"
      ]
