{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorEngine.Types
  ( OrchestratedOutput (..),
    defaultOrchestratedOutput,
    WindowRead (..),
    eventsCounterName,
  )
where

import Data.Default.Class (Default (..))
import Kernel.Prelude
import Lib.BehaviorTracker.Types (CounterValues)
import Lib.CommunicationEngine.Types (CommunicationDirective)
import Lib.ConsequenceEngine.Types (ConsequenceDirective)

-- | How the rulebook version picked for one evaluation reads its counters.
data WindowRead
  = -- | Time version (daily/weekly): the snapshot is evaluated as it is.
    TimeWindow
  | -- | Events version: the entity's last X events, added to the snapshot as
    -- @counters.events@ next to the daily/weekly counters and cooldowns.
    EventsWindow CounterValues
  | -- | Events version whose counts couldn't be read. The text names the cause;
    -- no rules run for this event.
    EventsUnavailable Text
  deriving (Show)

-- | Snapshot key for the events window: rules read @counters.events.actionCount@.
eventsCounterName :: Text
eventsCounterName = "events"

-- | Final output from rule evaluation — consumed by consequence + communication engines
data OrchestratedOutput = OrchestratedOutput
  { consequences :: [ConsequenceDirective],
    communications :: [CommunicationDirective]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Default OrchestratedOutput where
  def =
    OrchestratedOutput
      { consequences = [],
        communications = []
      }

-- | Default empty output
defaultOrchestratedOutput :: OrchestratedOutput
defaultOrchestratedOutput = def
