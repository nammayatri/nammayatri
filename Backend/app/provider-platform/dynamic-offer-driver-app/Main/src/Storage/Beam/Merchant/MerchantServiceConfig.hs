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

module Storage.Beam.Merchant.MerchantServiceConfig where

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
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

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

instance FromField Domain.ServiceName where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.ServiceName where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.ServiceName

instance FromBackendRow Postgres Domain.ServiceName

instance IsString Domain.ServiceName where
  fromString = show

data MerchantServiceConfigT f = MerchantServiceConfigT
  { merchantId :: B.C f Text,
    serviceName :: B.C f Domain.ServiceName,
    configJSON :: B.C f A.Value,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceConfigT where
  data PrimaryKey MerchantServiceConfigT f
    = Id (B.C f Domain.ServiceName)
    deriving (Generic, B.Beamable)
  primaryKey = Id . serviceName

instance ModelMeta MerchantServiceConfigT where
  modelFieldModification = merchantServiceConfigTMod
  modelTableName = "merchant_service_config"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MerchantServiceConfig = MerchantServiceConfigT Identity

instance FromJSON MerchantServiceConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MerchantServiceConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MerchantServiceConfig

merchantServiceConfigTMod :: MerchantServiceConfigT (B.FieldModification (B.TableField MerchantServiceConfigT))
merchantServiceConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      serviceName = B.fieldNamed "service_name",
      configJSON = B.fieldNamed "config_json",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

defaultMerchantServiceConfig :: MerchantServiceConfig
defaultMerchantServiceConfig =
  MerchantServiceConfigT
    { merchantId = "",
      serviceName = "",
      configJSON = "",
      updatedAt = defaultUTCDate,
      createdAt = defaultUTCDate
    }

instance Serialize MerchantServiceConfig where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantServiceConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceConfigToHSModifiers =
  M.empty

merchantServiceConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceConfigToPSModifiers =
  M.empty

$(enableKVPG ''MerchantServiceConfigT ['serviceName, 'merchantId] [])
