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

module Storage.Beam.Merchant where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Base64
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Base64 where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Base64 where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Base64

instance FromBackendRow Postgres Base64

instance FromField GeoRestriction where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be GeoRestriction where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be GeoRestriction

instance FromBackendRow Postgres GeoRestriction

-- deriving stock instance Read GeoRestriction

deriving stock instance Eq GeoRestriction

deriving stock instance Ord GeoRestriction

deriving stock instance Ord Base64

deriving stock instance Read Base64

instance IsString GeoRestriction where
  fromString = show

data MerchantT f = MerchantT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    subscriberId :: B.C f Text,
    name :: B.C f Text,
    city :: B.C f Text,
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    gatewayUrl :: B.C f Text,
    registryUrl :: B.C f Text,
    driverOfferBaseUrl :: B.C f Text,
    driverOfferApiKey :: B.C f Text,
    driverOfferMerchantId :: B.C f Text,
    geoHashPrecisionValue :: B.C f Int,
    signingPublicKey :: B.C f Base64,
    cipherText :: B.C f (Maybe Base64),
    signatureExpiry :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
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
  modelSchemaName = Just "atlas_app"

type Merchant = MerchantT Identity

instance FromJSON Merchant where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Merchant where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Merchant

merchantTMod :: MerchantT (B.FieldModification (B.TableField MerchantT))
merchantTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      shortId = B.fieldNamed "short_id",
      subscriberId = B.fieldNamed "subscriber_id",
      name = B.fieldNamed "name",
      city = B.fieldNamed "city",
      originRestriction = B.fieldNamed "origin_restriction",
      destinationRestriction = B.fieldNamed "destination_restriction",
      gatewayUrl = B.fieldNamed "gateway_url",
      registryUrl = B.fieldNamed "registry_url",
      driverOfferBaseUrl = B.fieldNamed "driver_offer_base_url",
      driverOfferApiKey = B.fieldNamed "driver_offer_api_key",
      driverOfferMerchantId = B.fieldNamed "driver_offer_merchant_id",
      geoHashPrecisionValue = B.fieldNamed "geo_hash_precision_value",
      signingPublicKey = B.fieldNamed "signing_public_key",
      cipherText = B.fieldNamed "cipher_text",
      signatureExpiry = B.fieldNamed "signature_expiry",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

defaultMerchant :: Merchant
defaultMerchant =
  MerchantT
    { id = "",
      shortId = "",
      subscriberId = "",
      name = "",
      city = "",
      originRestriction = "",
      destinationRestriction = "",
      gatewayUrl = "",
      registryUrl = "",
      driverOfferBaseUrl = "",
      driverOfferApiKey = "",
      driverOfferMerchantId = "",
      geoHashPrecisionValue = 0,
      signingPublicKey = "",
      cipherText = Nothing,
      signatureExpiry = 0,
      updatedAt = defaultUTCDate,
      createdAt = defaultUTCDate
    }

instance Serialize Merchant where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantToHSModifiers =
  M.empty

merchantToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantToPSModifiers =
  M.empty

$(enableKVPG ''MerchantT ['id] [])
