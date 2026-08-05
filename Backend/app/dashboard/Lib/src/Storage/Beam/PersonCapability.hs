{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.PersonCapability where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.Capability as DC
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data PersonCapabilityT f = PersonCapabilityT
  { personId :: B.C f Text,
    capabilityId :: B.C f Text,
    mode :: B.C f DC.CapabilityMode,
    reason :: B.C f Text,
    grantedBy :: B.C f (Maybe Text),
    expiresAt :: B.C f (Maybe Time.UTCTime),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonCapabilityT where
  data PrimaryKey PersonCapabilityT f
    = PersonCapabilityKey (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey PersonCapabilityT {..} = PersonCapabilityKey personId capabilityId

type PersonCapability = PersonCapabilityT Identity

$(enableKVPG ''PersonCapabilityT ['personId, 'capabilityId] [])

$(mkTableInstancesGenericSchema ''PersonCapabilityT "person_capability")
