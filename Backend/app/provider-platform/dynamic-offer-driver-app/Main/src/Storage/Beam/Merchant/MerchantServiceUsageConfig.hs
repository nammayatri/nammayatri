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
import qualified Data.Text as T
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
import Kernel.External.AadhaarVerification.Types
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types
import Kernel.External.SMS.Types
import Kernel.External.Ticket.Types
import Kernel.External.Verification.Types
import Kernel.External.Whatsapp.Types
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField VerificationService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VerificationService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be VerificationService

instance FromBackendRow Postgres VerificationService

instance FromField AadhaarVerificationService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AadhaarVerificationService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AadhaarVerificationService

instance FromBackendRow Postgres AadhaarVerificationService

instance IsString AadhaarVerificationService where
  fromString = show

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

instance FromField IssueTicketService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IssueTicketService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IssueTicketService

instance FromBackendRow Postgres IssueTicketService

instance IsString IssueTicketService where
  fromString = show

fromFieldSmsService ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [SmsService]
fromFieldSmsService f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

fromFieldWhatsappService ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [WhatsappService]
fromFieldWhatsappService f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

instance HasSqlValueSyntax be (V.Vector Text) => HasSqlValueSyntax be [SmsService] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList (T.pack . show <$> x))

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [SmsService]

instance FromBackendRow Postgres [SmsService]

instance FromField WhatsappService where
  fromField = fromFieldEnum

instance FromField [WhatsappService] where
  fromField = fromFieldWhatsappService

instance HasSqlValueSyntax be (V.Vector Text) => HasSqlValueSyntax be [WhatsappService] where
  sqlValueSyntax x = sqlValueSyntax (V.fromList (T.pack . show <$> x))

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [WhatsappService]

instance FromBackendRow Postgres [WhatsappService]

instance IsString CallService where
  fromString = show

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Text,
    initiateCall :: B.C f CallService,
    getDistances :: B.C f (SMapsService 'GetDistances),
    getDistancesPercentage :: B.C f Text,
    getEstimatedPickupDistances :: B.C f (SMapsService 'GetEstimatedPickupDistances),
    getEstimatedPickupDistancesPercentage :: B.C f Text,
    getRoutes :: B.C f (SMapsService 'GetRoutes),
    getRoutesPercentage :: B.C f Text,
    getPickupRoutes :: B.C f (SMapsService 'GetPickupRoutes),
    getPickupRoutesPercentage :: B.C f Text,
    getTripRoutes :: B.C f (SMapsService 'GetTripRoutes),
    getTripRoutesPercentage :: B.C f Text,
    snapToRoad :: B.C f (SMapsService 'SnapToRoad),
    snapToRoadPercentage :: B.C f Text,
    getPlaceName :: B.C f (SMapsService 'GetPlaceName),
    getPlaceNamePercentage :: B.C f Text,
    getPlaceDetails :: B.C f (SMapsService 'GetPlaceDetails),
    getPlaceDetailsPercentage :: B.C f Text,
    autoComplete :: B.C f (SMapsService 'AutoComplete),
    autoCompletePercentage :: B.C f Text,
    getDistancesForCancelRide :: B.C f (SMapsService 'GetDistancesForCancelRide),
    getDistancesForCancelRidePercentage :: B.C f Text,
    smsProvidersPriorityList :: B.C f [SmsService],
    whatsappProvidersPriorityList :: B.C f [WhatsappService],
    verificationService :: B.C f VerificationService,
    faceVerificationService :: B.C f VerificationService,
    aadhaarVerificationService :: B.C f AadhaarVerificationService,
    issueTicketService :: B.C f IssueTicketService,
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
      getDistancesPercentage = B.fieldNamed "get_distances_percentage",
      getEstimatedPickupDistances = B.fieldNamed "get_estimated_pickup_distances",
      getEstimatedPickupDistancesPercentage = B.fieldNamed "get_estimated_pickup_distances_percentage",
      getRoutes = B.fieldNamed "get_routes",
      getRoutesPercentage = B.fieldNamed "get_routes_percentage",
      getPickupRoutes = B.fieldNamed "get_pickup_routes",
      getPickupRoutesPercentage = B.fieldNamed "get_pickup_routes_percentage",
      getTripRoutes = B.fieldNamed "get_trip_routes",
      getTripRoutesPercentage = B.fieldNamed "get_trip_routes_percentage",
      snapToRoad = B.fieldNamed "snap_to_road",
      snapToRoadPercentage = B.fieldNamed "snap_to_road_percentage",
      getPlaceName = B.fieldNamed "get_place_name",
      getPlaceNamePercentage = B.fieldNamed "get_place_name_percentage",
      getPlaceDetails = B.fieldNamed "get_place_details",
      getPlaceDetailsPercentage = B.fieldNamed "get_place_details_percentage",
      autoComplete = B.fieldNamed "auto_complete",
      autoCompletePercentage = B.fieldNamed "auto_complete_percentage",
      getDistancesForCancelRide = B.fieldNamed "get_distances_for_cancel_ride",
      getDistancesForCancelRidePercentage = B.fieldNamed "get_distances_for_cancel_ride_percentage",
      smsProvidersPriorityList = B.fieldNamed "sms_providers_priority_list",
      whatsappProvidersPriorityList = B.fieldNamed "whatsapp_providers_priority_list",
      verificationService = B.fieldNamed "verification_service",
      faceVerificationService = B.fieldNamed "face_verification_service",
      aadhaarVerificationService = B.fieldNamed "aadhaar_verification_service",
      issueTicketService = B.fieldNamed "issue_ticket_service",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

instance IsString MapsService where
  fromString = show

instance IsString VerificationService where
  fromString = show

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
