{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The typed surge table (dev/docs/fare-policy-revamp-plan.md, Phase 4).
-- Replaces DYNAMIC_PRICING_UNIFIED json-logic: rows are evaluated top-down,
-- FIRST MATCH WINS. A bound on a signal only matches when the signal is
-- PRESENT — a missing signal (cold Redis key) never satisfies a bounded row,
-- so "no data" can never read as "maximum scarcity". A row with no bounds
-- always matches and acts as the default row.
module Domain.Types.Extra.SurgeConfig where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (Centesimal)

-- The surge_config.rows column stores the whole list as ONE JSON text value
-- via the spec's toTType/fromTType codec (beamType Text). Do NOT add beam
-- instances for [SurgeRow] here: the KV drainer serializes list columns
-- element-wise into a Postgres array literal, so any whole-list
-- HasSqlValueSyntax instance diverges from what the drainer writes and the
-- column ends up unreadable (BeamRowReadError).
data SurgeRow = SurgeRow
  { -- inclusive-min / exclusive-max bounds; Nothing = unbounded on that side
    qarMin :: Maybe Double,
    qarMax :: Maybe Double,
    supplyDemandRatioMin :: Maybe Double,
    supplyDemandRatioMax :: Maybe Double,
    distanceKmMin :: Maybe Int,
    distanceKmMax :: Maybe Int,
    -- outputs; at least one must be set (validated at write time)
    congestionMultiplier :: Maybe Centesimal,
    congestionPerMinCharge :: Maybe Double
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
