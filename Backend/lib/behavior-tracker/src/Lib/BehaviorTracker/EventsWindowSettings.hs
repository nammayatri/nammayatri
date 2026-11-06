{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fixed limits for events-window behaviour rulebooks ("offences in the last X events").
-- Changing either is a code change and a deploy; the control center's editor mirrors
-- maxWindowSize (EVENTS_WINDOW_MAX in chitragupta/utils/rulebookSpec.ts), so change both.
module Lib.BehaviorTracker.EventsWindowSettings
  ( maxWindowSize,
    oversizedWindowLength,
    windowTtlDays,
  )
where

import Kernel.Prelude

-- | Largest X a rulebook version may save, and how many events every window keeps
-- (whatever X is live), so raising X later already finds the history. One character per
-- event: at most this many bytes per entity and action type.
maxWindowSize :: Int
maxWindowSize = 1000

-- | A stored window longer than this was not written by the window scripts (they always
-- trim to at most 'maxWindowSize'), so it is corrupt: the scripts start it fresh instead of
-- working through it, and the caller logs it. Keeps a bad key from slowing Redis down.
oversizedWindowLength :: Int
oversizedWindowLength = 2 * maxWindowSize

-- | A window expires after this many days without an event.
windowTtlDays :: Int
windowTtlDays = 60
