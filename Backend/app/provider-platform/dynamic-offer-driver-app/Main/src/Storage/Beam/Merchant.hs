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

module Storage.Beam.Merchant where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
-- import Database.Beam.Postgres
--   ( Postgres,
--   )
-- import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Geofencing
import Lib.Utils
import Lib.UtilsTH
import Sequelize

fromFieldEnum' ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion GeoRestriction
fromFieldEnum' f mbValue = case mbValue of
  Nothing -> pure Unrestricted
  Just _ -> Regions . V.toList <$> fromField f mbValue

instance FromField GeoRestriction where
  fromField = fromFieldEnum'

instance HasSqlValueSyntax be String => HasSqlValueSyntax be GeoRestriction where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be GeoRestriction

instance FromBackendRow Postgres GeoRestriction

instance FromField Domain.Status where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Status where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Status

instance FromBackendRow Postgres Domain.Status

instance IsString Domain.Status where
  fromString = show

instance IsString GeoRestriction where
  fromString = show

data MerchantT f = MerchantT
  { id :: B.C f Text,
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    subscriberId :: B.C f Text,
    uniqueKeyId :: B.C f Text,
    shortId :: B.C f Text,
    mobileNumber :: B.C f (Maybe Text),
    mobileCountryCode :: B.C f (Maybe Text),
    gstin :: B.C f (Maybe Text),
    fromTime :: B.C f (Maybe Time.UTCTime),
    toTime :: B.C f (Maybe Time.UTCTime),
    headCount :: B.C f (Maybe Int),
    status :: B.C f Domain.Status,
    city :: B.C f Text,
    verified :: B.C f Bool,
    enabled :: B.C f Bool,
    internalApiKey :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta MerchantT where
  modelFieldModification = merchantTMod
  modelTableName = "merchant"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Merchant = MerchantT Identity

instance FromJSON Merchant where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Merchant where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Merchant

deriving stock instance Ord Domain.Status

deriving stock instance Read GeoRestriction

deriving stock instance Ord GeoRestriction

deriving stock instance Eq GeoRestriction

merchantTMod :: MerchantT (B.FieldModification (B.TableField MerchantT))
merchantTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      name = B.fieldNamed "name",
      description = B.fieldNamed "description",
      subscriberId = B.fieldNamed "subscriber_id",
      uniqueKeyId = B.fieldNamed "unique_key_id",
      shortId = B.fieldNamed "short_id",
      mobileNumber = B.fieldNamed "mobile_number",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      gstin = B.fieldNamed "gstin",
      fromTime = B.fieldNamed "from_time",
      toTime = B.fieldNamed "to_time",
      headCount = B.fieldNamed "head_count",
      status = B.fieldNamed "status",
      city = B.fieldNamed "city",
      verified = B.fieldNamed "verified",
      enabled = B.fieldNamed "enabled",
      internalApiKey = B.fieldNamed "internal_api_key",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      originRestriction = B.fieldNamed "origin_restriction",
      destinationRestriction = B.fieldNamed "destination_restriction",
      info = B.fieldNamed "info"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantToHSModifiers =
  M.empty

merchantToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantToPSModifiers =
  M.empty

defaultMerchant :: Merchant
defaultMerchant =
  MerchantT
    { id = "",
      name = "",
      description = Nothing,
      subscriberId = "",
      uniqueKeyId = "",
      shortId = "",
      mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      gstin = Nothing,
      fromTime = Nothing,
      toTime = Nothing,
      headCount = Nothing,
      status = "",
      city = "",
      verified = False,
      enabled = False,
      internalApiKey = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate,
      originRestriction = "",
      destinationRestriction = "",
      info = Nothing
    }

instance Serialize Merchant where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''MerchantT ['id] [])
