{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DriverOnboarding.DriverRCAssociation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as BST
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data DriverRCAssociationT f = DriverRCAssociationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    rcId :: B.C f Text,
    associatedOn :: B.C f Time.UTCTime,
    associatedTill :: B.C f (Maybe Time.UTCTime),
    consent :: B.C f Bool,
    consentTimestamp :: B.C f Time.UTCTime,
    isRcActive :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRCAssociationT where
  data PrimaryKey DriverRCAssociationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverRCAssociationT where
  modelFieldModification = driverRCAssociationTMod
  modelTableName = "driver_rc_association"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverRCAssociation = DriverRCAssociationT Identity

driverRCAssociationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverRCAssociationT)
driverRCAssociationTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "driver_rc_association"
    <> B.modifyTableFields driverRCAssociationTMod

instance FromJSON DriverRCAssociation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverRCAssociation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverRCAssociation

driverRCAssociationTMod :: DriverRCAssociationT (B.FieldModification (B.TableField DriverRCAssociationT))
driverRCAssociationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      rcId = B.fieldNamed "rc_id",
      associatedOn = B.fieldNamed "associated_on",
      associatedTill = B.fieldNamed "associated_till",
      consent = B.fieldNamed "consent",
      consentTimestamp = B.fieldNamed "consent_timestamp",
      isRcActive = B.fieldNamed "is_rc_active"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverRcAssociationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverRcAssociationToHSModifiers =
  M.empty

driverRcAssociationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverRcAssociationToPSModifiers =
  M.empty

instance Serialize DriverRCAssociation where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverRCAssociationT ['id] [['driverId], ['rcId]])
