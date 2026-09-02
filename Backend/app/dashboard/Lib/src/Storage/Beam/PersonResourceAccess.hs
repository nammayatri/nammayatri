{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.PersonResourceAccess where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.ResourceScope as DRS
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)

data PersonResourceAccessT f = PersonResourceAccessT
  { id :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    operatingCity :: B.C f City,
    resourceType :: B.C f DRS.ResourceType,
    resourceId :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonResourceAccessT where
  data PrimaryKey PersonResourceAccessT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PersonResourceAccess = PersonResourceAccessT Identity

$(enableKVPG ''PersonResourceAccessT ['id] [])

$(mkTableInstancesGenericSchema ''PersonResourceAccessT "person_resource_access")
