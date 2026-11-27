{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FleetScopeMode where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

-- | How a merchant treats scoped role assignments, per merchant so it can roll out one at a time.
--
--   * OFF: assignments are ignored. This is how every merchant starts.
--   * SHADOW: the permission check also works out the answer from assignments, compares it with the
--     old answer, and only logs the difference.
--   * ENFORCE: the answer from assignments is the one that applies.
data FleetScopeMode
  = OFF
  | SHADOW
  | ENFORCE
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''FleetScopeMode)
