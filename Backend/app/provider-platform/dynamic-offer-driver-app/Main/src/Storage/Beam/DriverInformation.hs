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

module Storage.Beam.DriverInformation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
-- import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as BST
-- import Database.Beam.Postgres
--   ( Postgres,
--   )
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.DriverInformation as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
-- import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data DriverInformationT f = DriverInformationT
  { driverId :: B.C f Text,
    adminId :: B.C f (Maybe Text),
    merchantId :: B.C f (Maybe Text),
    active :: B.C f Bool,
    onRide :: B.C f Bool,
    enabled :: B.C f Bool,
    blocked :: B.C f Bool,
    numOfLocks :: B.C f Int,
    verified :: B.C f Bool,
    subscribed :: B.C f Bool,
    paymentPending :: B.C f Bool,
    aadhaarVerified :: B.C f Bool,
    lastEnabledOn :: B.C f (Maybe Time.UTCTime),
    referralCode :: B.C f (Maybe Text),
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    blockedReason :: B.C f (Maybe Text),
    blockExpiryTime :: B.C f (Maybe Time.UTCTime),
    mode :: B.C f (Maybe Domain.DriverMode),
    autoPayStatus :: B.C f (Maybe Domain.DriverAutoPayStatus),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta DriverInformationT where
  modelFieldModification = driverInformationTMod
  modelTableName = "driver_information"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverInformation = DriverInformationT Identity

dInformationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverInformationT)
dInformationTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "driver_information"
    <> B.modifyTableFields driverInformationTMod

instance FromJSON DriverInformation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverInformation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverInformation

driverInformationTMod :: DriverInformationT (B.FieldModification (B.TableField DriverInformationT))
driverInformationTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      adminId = B.fieldNamed "admin_id",
      merchantId = B.fieldNamed "merchant_id",
      active = B.fieldNamed "active",
      onRide = B.fieldNamed "on_ride",
      enabled = B.fieldNamed "enabled",
      blocked = B.fieldNamed "blocked",
      verified = B.fieldNamed "verified",
      subscribed = B.fieldNamed "subscribed",
      paymentPending = B.fieldNamed "payment_pending",
      aadhaarVerified = B.fieldNamed "aadhaar_verified",
      numOfLocks = B.fieldNamed "num_of_locks",
      lastEnabledOn = B.fieldNamed "last_enabled_on",
      blockedReason = B.fieldNamed "blocked_reason",
      blockExpiryTime = B.fieldNamed "block_expiry_time",
      referralCode = B.fieldNamed "referral_code",
      canDowngradeToSedan = B.fieldNamed "can_downgrade_to_sedan",
      canDowngradeToHatchback = B.fieldNamed "can_downgrade_to_hatchback",
      canDowngradeToTaxi = B.fieldNamed "can_downgrade_to_taxi",
      mode = B.fieldNamed "mode",
      autoPayStatus = B.fieldNamed "auto_pay_status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverInformationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverInformationToHSModifiers =
  M.empty

driverInformationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverInformationToPSModifiers =
  M.empty

instance Serialize DriverInformation where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverInformationT ['driverId] [])
