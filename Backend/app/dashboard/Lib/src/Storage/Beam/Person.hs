{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Person where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.Person.Type as DPT
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f Text,
    lastName :: B.C f Text,
    roleId :: B.C f Text,
    emailEncrypted :: B.C f (Maybe Text),
    emailHash :: B.C f (Maybe DbHash),
    mobileNumberEncrypted :: B.C f Text,
    mobileNumberHash :: B.C f DbHash,
    mobileCountryCode :: B.C f Text,
    passwordHash :: B.C f (Maybe DbHash),
    dashboardAccessType :: B.C f (Maybe DRole.DashboardAccessType),
    dashboardType :: B.C f DPT.DashboardType, -- Using enum for type safety
    receiveNotification :: B.C f (Maybe Bool),
    verified :: B.C f (Maybe Bool),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    rejectionReason :: B.C f (Maybe Text),
    rejectedAt :: B.C f (Maybe Time.UTCTime),
    passwordUpdatedAt :: B.C f (Maybe Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [])

$(mkTableInstancesGenericSchema ''PersonT "person")
