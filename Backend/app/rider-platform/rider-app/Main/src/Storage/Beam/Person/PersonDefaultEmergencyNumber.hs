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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Person.PersonDefaultEmergencyNumber where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Lib.UtilsTH
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

instance ModelMeta PersonDefaultEmergencyNumberT where
  modelFieldModification = personDefaultEmergencyNumberTMod
  modelTableName = "person_default_emergency_number"
  modelSchemaName = Just "atlas_app"

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

instance FromJSON PersonDefaultEmergencyNumber where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON PersonDefaultEmergencyNumber where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show PersonDefaultEmergencyNumber

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

instance Serialize PersonDefaultEmergencyNumber where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

personDefaultEmergencyNumberToHSModifiers :: M.Map Text (A.Value -> A.Value)
personDefaultEmergencyNumberToHSModifiers =
  M.empty

personDefaultEmergencyNumberToPSModifiers :: M.Map Text (A.Value -> A.Value)
personDefaultEmergencyNumberToPSModifiers =
  M.empty

$(enableKVPG ''PersonDefaultEmergencyNumberT ['personId] [])
