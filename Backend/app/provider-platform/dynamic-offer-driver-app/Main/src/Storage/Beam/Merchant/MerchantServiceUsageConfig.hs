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

module Storage.Beam.Merchant.MerchantServiceUsageConfig where

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
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types
import Kernel.External.SMS.Types
import Kernel.External.Verification.Types
import Kernel.External.Whatsapp.Types
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

instance FromField VerificationService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VerificationService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be VerificationService

instance FromBackendRow Postgres VerificationService

instance FromField MapsService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MapsService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MapsService

instance FromBackendRow Postgres MapsService

instance FromField CallService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CallService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CallService

instance FromBackendRow Postgres CallService

instance FromField [SmsService] where
  fromField = fromFieldSmsService

instance FromField SmsService where
  fromField = fromFieldEnum

fromFieldSmsService ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [SmsService]
fromFieldSmsService f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

-- Nothing -> pure Unrestricted
-- Just _ -> (Regions . V.toList) <$> (fromField f mbValue)

fromFieldWhatsappService ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [WhatsappService]
fromFieldWhatsappService f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [SmsService] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [SmsService]

instance FromBackendRow Postgres [SmsService]

-- instance IsString [SmsService] where
--   fromString = show
-- instance IsString [WhatsappService] where
--   fromString = show

instance FromField WhatsappService where
  fromField = fromFieldEnum

instance FromField [WhatsappService] where
  fromField = fromFieldWhatsappService

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [WhatsappService] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [WhatsappService]

instance FromBackendRow Postgres [WhatsappService]

instance IsString CallService where
  fromString = show

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Text,
    initiateCall :: B.C f CallService,
    getDistances :: B.C f MapsService,
    getEstimatedPickupDistances :: B.C f MapsService,
    getRoutes :: B.C f MapsService,
    getPickupRoutes :: B.C f MapsService,
    getTripRoutes :: B.C f MapsService,
    snapToRoad :: B.C f MapsService,
    getPlaceName :: B.C f MapsService,
    getPlaceDetails :: B.C f MapsService,
    autoComplete :: B.C f MapsService,
    getDistancesForCancelRide :: B.C f MapsService,
    smsProvidersPriorityList :: B.C f [SmsService],
    whatsappProvidersPriorityList :: B.C f [WhatsappService],
    verificationService :: B.C f VerificationService,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta MerchantServiceUsageConfigT where
  modelFieldModification = merchantServiceUsageConfigTMod
  modelTableName = "merchant_service_usage_config"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

instance FromJSON MerchantServiceUsageConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MerchantServiceUsageConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MerchantServiceUsageConfig

merchantServiceUsageConfigTMod :: MerchantServiceUsageConfigT (B.FieldModification (B.TableField MerchantServiceUsageConfigT))
merchantServiceUsageConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      initiateCall = B.fieldNamed "initiate_call",
      getDistances = B.fieldNamed "get_distances",
      getEstimatedPickupDistances = B.fieldNamed "get_estimated_pickup_distances",
      getRoutes = B.fieldNamed "get_routes",
      getPickupRoutes = B.fieldNamed "get_pickup_routes",
      getTripRoutes = B.fieldNamed "get_trip_routes",
      snapToRoad = B.fieldNamed "snap_to_road",
      getPlaceName = B.fieldNamed "get_place_name",
      getPlaceDetails = B.fieldNamed "get_place_details",
      autoComplete = B.fieldNamed "auto_complete",
      getDistancesForCancelRide = B.fieldNamed "get_distances_for_cancel_ride",
      smsProvidersPriorityList = B.fieldNamed "sms_providers_priority_list",
      whatsappProvidersPriorityList = B.fieldNamed "whatsapp_providers_priority_list",
      verificationService = B.fieldNamed "verification_service",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

instance IsString MapsService where
  fromString = show

instance IsString VerificationService where
  fromString = show

-- defaultMerchantServiceUsageConfig :: MerchantServiceUsageConfig
-- defaultMerchantServiceUsageConfig =
--   MerchantServiceUsageConfigT
--     { merchantId = "",
--       initiateCall = "",
--       getDistances = "",
--       getEstimatedPickupDistances = "",
--       getRoutes = "",
--       getPickupRoutes = "",
--       getTripRoutes = "",
--       snapToRoad = "",
--       getPlaceName = "",
--       getPlaceDetails = "",
--       autoComplete = "",
--       smsProvidersPriorityList = "",
--       whatsappProvidersPriorityList = "",
--       verificationService = "",
--       updatedAt = defaultUTCDate,
--       createdAt = defaultUTCDate
--     }

instance Serialize MerchantServiceUsageConfig where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantServiceUsageConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceUsageConfigToHSModifiers =
  M.empty

merchantServiceUsageConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceUsageConfigToPSModifiers =
  M.empty

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantId] [])
