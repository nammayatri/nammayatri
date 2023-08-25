{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Beam.Person.PersonDefaultEmergencyNumber where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data PersonDefaultEmergencyNumberT f = PersonDefaultEmergencyNumberT
  { personId :: B.C f Text,
    name :: B.C f Text,
    mobileCountryCode :: B.C f Text,
    mobileNumberEncrypted :: B.C f Text,
    mobileNumberHash :: B.C f DbHash,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonDefaultEmergencyNumberT where
  data PrimaryKey PersonDefaultEmergencyNumberT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

personDefaultEmergencyNumberTMod :: PersonDefaultEmergencyNumberT (B.FieldModification (B.TableField PersonDefaultEmergencyNumberT))
personDefaultEmergencyNumberTMod =
  B.tableModification
    { personId = B.fieldNamed "person_id",
      name = B.fieldNamed "name",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      mobileNumberEncrypted = B.fieldNamed "mobile_number_encrypted",
      mobileNumberHash = B.fieldNamed "mobile_number_hash",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''PersonDefaultEmergencyNumberT ['personId] [])

$(mkTableInstances ''PersonDefaultEmergencyNumberT "person_default_emergency_number" "atlas_app")
