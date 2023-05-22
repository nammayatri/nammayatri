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
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Person (PersonTId)

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Domain.DriverMode where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DriverMode where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DriverMode

instance FromBackendRow Postgres Domain.DriverMode

data DriverInformationT f = DriverInformationT
  { driverId :: B.C f Text,
    adminId :: B.C f (Maybe Text),
    active :: B.C f Bool,
    onRide :: B.C f Bool,
    enabled :: B.C f Bool,
    blocked :: B.C f Bool,
    verified :: B.C f Bool,
    lastEnabledOn :: B.C f (Maybe Time.UTCTime),
    referralCode :: B.C f (Maybe Text),
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    mode :: B.C f (Maybe Domain.DriverMode),
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
  mkExprWithDefault _ = B.insertExpressions []

type DriverInformation = DriverInformationT Identity

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
      active = B.fieldNamed "active",
      onRide = B.fieldNamed "on_ride",
      enabled = B.fieldNamed "enabled",
      blocked = B.fieldNamed "blocked",
      verified = B.fieldNamed "verified",
      lastEnabledOn = B.fieldNamed "last_enabled_on",
      referralCode = B.fieldNamed "referral_code",
      canDowngradeToSedan = B.fieldNamed "can_downgrade_to_sedan",
      canDowngradeToHatchback = B.fieldNamed "can_downgrade_to_hatchback",
      canDowngradeToTaxi = B.fieldNamed "can_downgrade_to_taxi",
      mode = B.fieldNamed "mode",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverInformationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverInformationToHSModifiers =
  M.fromList
    []

driverInformationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverInformationToPSModifiers =
  M.fromList
    []

defaultDriverInformation :: DriverInformation
defaultDriverInformation =
  DriverInformationT
    { driverId = "",
      adminId = Nothing,
      active = False,
      onRide = False,
      enabled = False,
      blocked = False,
      verified = False,
      lastEnabledOn = Nothing,
      referralCode = Nothing,
      canDowngradeToSedan = False,
      canDowngradeToHatchback = False,
      canDowngradeToTaxi = False,
      mode = Nothing,
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize DriverInformation where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverInformationT ['driverId] [])
