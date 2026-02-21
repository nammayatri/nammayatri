module SharedLogic.FRFSSeatBooking.Lua where

import qualified Data.ByteString.Char8 as B
import Kernel.Prelude

-- | Atomically checks availability and holds multiple seats
--   for a single trip and segment range.
--
--   This script guarantees:
--   • All seats are checked for availability first (no mutation).
--   • If ANY seat is unavailable → returns 0 and performs NO writes.
--   • If ALL seats are available → sets bits for the range on all seats,
--     writes hold metadata, and creates an expiry timer.
--
--   Cluster Safety:
--   • All KEYS must share the same Redis hash slot.
--   • This is ensured via tripTag {tripId} usage in key construction.
--
--   KEYS:
--     [ seatKey1
--     , seatKey2
--     , ...
--     , metaKey
--     , timerKey
--     ]
--
--   ARGV:
--     [ holdId
--     , ttlSeconds
--     , metaJSON
--     , fromStopIdx
--     , toStopIdx
--     , seatCount
--     ]
--
--   Returns:
--     1 → Success
--     0 → Conflict (at least one seat already booked)
holdSeatScript :: B.ByteString
holdSeatScript =
  B.pack $
    unlines
      [ "-- ARGV[1] = holdId",
        "-- ARGV[2] = ttl (seconds)",
        "-- ARGV[3] = meta JSON",
        "-- ARGV[4] = fromIdx",
        "-- ARGV[5] = toIdx",
        "-- ARGV[6] = seatCount",
        "",
        "local holdId = ARGV[1]",
        "local ttl = tonumber(ARGV[2])",
        "local meta = ARGV[3]",
        "local fromIdx = tonumber(ARGV[4])",
        "local toIdx = tonumber(ARGV[5])",
        "local seatCount = tonumber(ARGV[6])",
        "",
        "-- =========================================================",
        "-- 1. Availability Check Phase (NO MUTATION)",
        "--    We first verify that ALL requested seats are free",
        "--    for the entire segment range.",
        "--    If ANY bit is set → abort immediately.",
        "-- =========================================================",
        "for i = 1, seatCount do",
        "  local seatKey = KEYS[i]",
        "  local curr = fromIdx",
        "",
        "  while curr < toIdx do",
        "    -- Read in 32-bit chunks to avoid Lua float precision issues",
        "    local chunk = math.min(toIdx - curr, 32)",
        "    local val = redis.call('BITFIELD', seatKey, 'GET', 'u' .. chunk, curr)[1]",
        "",
        "    -- If any bit is already set → seat not available",
        "    if val > 0 then",
        "      return 0",
        "    end",
        "",
        "    curr = curr + chunk",
        "  end",
        "end",
        "",
        "-- =========================================================",
        "-- 2. Mutation Phase",
        "--    Since ALL seats are free, we now set bits",
        "--    for the requested range across ALL seats.",
        "-- =========================================================",
        "for i = 1, seatCount do",
        "  local seatKey = KEYS[i]",
        "  local curr = fromIdx",
        "",
        "  while curr < toIdx do",
        "    local chunk = math.min(toIdx - curr, 32)",
        "",
        "    -- Create bitmask with 'chunk' number of 1s",
        "    -- Example: chunk=3 → mask=111b",
        "    local mask = math.pow(2, chunk) - 1",
        "",
        "    redis.call('BITFIELD', seatKey, 'SET', 'u' .. chunk, curr, mask)",
        "    curr = curr + chunk",
        "  end",
        "end",
        "",
        "-- =========================================================",
        "-- 3. Create Hold Metadata + Expiry Timer",
        "--    These keys are in same hash slot (tripTag)",
        "--    so this remains cluster-safe.",
        "-- =========================================================",
        "local metaKey = KEYS[seatCount + 1]",
        "local timerKey = KEYS[seatCount + 2]",
        "",
        "redis.call('SET', metaKey, meta)",
        "redis.call('SET', timerKey, '1', 'EX', ttl)",
        "",
        "-- Success",
        "return 1"
      ]

-- | Atomically clears a seat range for multiple seats.
--
--   This script is typically invoked by the hold reaper.
--   It assumes metadata validation has already occurred.
--
--   KEYS:
--     [ seatKey1
--     , seatKey2
--     , ...
--     ]
--
--   ARGV:
--     [ fromStopIdx
--     , toStopIdx
--     , seatCount
--     ]
--
--   Returns:
--     1 → Success
clearMultiScript :: B.ByteString
clearMultiScript =
  B.pack $
    unlines
      [ "-- ARGV[1] = fromIdx",
        "-- ARGV[2] = toIdx",
        "-- ARGV[3] = seatCount",
        "",
        "local fromIdx = tonumber(ARGV[1])",
        "local toIdx = tonumber(ARGV[2])",
        "local seatCount = tonumber(ARGV[3])",
        "",
        "-- =========================================================",
        "-- Clear bits for all seats in the hold",
        "-- This script assumes metadata was already validated",
        "-- and only performs mutation.",
        "-- =========================================================",
        "for i = 1, seatCount do",
        "  local seatKey = KEYS[i]",
        "  local curr = fromIdx",
        "",
        "  while curr < toIdx do",
        "    local chunk = math.min(toIdx - curr, 32)",
        "",
        "    -- Setting mask to 0 clears all bits in range",
        "    redis.call('BITFIELD', seatKey, 'SET', 'u' .. chunk, curr, 0)",
        "",
        "    curr = curr + chunk",
        "  end",
        "end",
        "",
        "-- Success",
        "return 1"
      ]
