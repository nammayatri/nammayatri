{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.EntityAccess where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data EntityAccessT f = EntityAccessT
  { id :: B.C f Text,
    personId :: B.C f Text,
    entityId :: B.C f Text,
    merchantId :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EntityAccessT where
  data PrimaryKey EntityAccessT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type EntityAccess = EntityAccessT Identity

$(enableKVPG ''EntityAccessT ['id] [['personId]])

$(mkTableInstancesGenericSchema ''EntityAccessT "entity_access")
