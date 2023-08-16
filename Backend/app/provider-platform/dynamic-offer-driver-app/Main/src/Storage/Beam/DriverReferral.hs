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

module Storage.Beam.DriverReferral where

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

data DriverReferralT f = DriverReferralT
  { referralCode :: B.C f Text,
    driverId :: B.C f Text,
    linkedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverReferralT where
  data PrimaryKey DriverReferralT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . referralCode

instance ModelMeta DriverReferralT where
  modelFieldModification = driverReferralTMod
  modelTableName = "driver_referral"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverReferral = DriverReferralT Identity

instance FromJSON DriverReferral where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverReferral where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverReferral

driverReferralTMod :: DriverReferralT (B.FieldModification (B.TableField DriverReferralT))
driverReferralTMod =
  B.tableModification
    { referralCode = B.fieldNamed "referral_code",
      driverId = B.fieldNamed "driver_id",
      linkedAt = B.fieldNamed "linked_at"
    }

driverReferralTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverReferralT)
driverReferralTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "driver_referral"
    <> B.modifyTableFields driverReferralTMod

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverReferralToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverReferralToHSModifiers =
  M.empty

driverReferralToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverReferralToPSModifiers =
  M.empty

instance Serialize DriverReferral where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverReferralT ['referralCode] [['driverId]])
