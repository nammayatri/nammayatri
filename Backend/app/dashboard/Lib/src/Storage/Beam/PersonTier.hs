{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.PersonTier where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

-- Narrow projection of the person table: only (id, admin_tier). Exists so the
-- admin-tier guards can read the column without widening the full PersonT
-- record (and every construction site across three apps) before the Phase 4
-- flip. Read-only by convention — admin_tier is written by migration/manual
-- seed only until the flip.
data PersonTierT f = PersonTierT
  { id :: B.C f Text,
    adminTier :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonTierT where
  data PrimaryKey PersonTierT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PersonTier = PersonTierT Identity

$(enableKVPG ''PersonTierT ['id] [])

$(mkTableInstancesGenericSchema ''PersonTierT "person")
