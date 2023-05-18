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

module Storage.Beam.DriverQuote where

import qualified Data.Aeson as A
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
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Centesimal, Meters (..))
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Common as Common
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)
import qualified Storage.Tabular.SearchRequest as SReq
import qualified Storage.Tabular.SearchRequestForDriver as SRFD
import Storage.Tabular.Vehicle ()

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Variant.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant.Variant

instance FromBackendRow Postgres Variant.Variant

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Domain.DriverQuoteStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DriverQuoteStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DriverQuoteStatus

instance FromBackendRow Postgres Domain.DriverQuoteStatus

instance FromField Common.Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Common.Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Common.Money

instance FromBackendRow Postgres Common.Money

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

data DriverQuoteT f = DriverQuoteT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    searchRequestId :: B.C f Text,
    searchRequestForDriverId :: B.C f (Maybe Text),
    driverId :: B.C f Text,
    driverName :: B.C f Text,
    driverRating :: B.C f (Maybe Centesimal),
    status :: B.C f Domain.DriverQuoteStatus,
    vehicleVariant :: B.C f Variant.Variant,
    distance :: B.C f Meters,
    distanceToPickup :: B.C f Meters,
    durationToPickup :: B.C f Double,
    validTill :: B.C f Time.LocalTime,
    estimatedFare :: B.C f Common.Money,
    fareParametersId :: B.C f Text,
    providerId :: B.C f Text,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.DriverQuoteStatus where
  fromString = show

instance IsString Variant.Variant where
  fromString = show

instance IsString Common.Money where
  fromString = show

instance IsString Meters where
  fromString = show

instance B.Table DriverQuoteT where
  data PrimaryKey DriverQuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverQuoteT where
  modelFieldModification = driverQuoteTMod
  modelTableName = "driver_quote"
  mkExprWithDefault _ = B.insertExpressions []

type DriverQuote = DriverQuoteT Identity

instance FromJSON DriverQuote where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverQuote where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.DriverQuoteStatus where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.DriverQuoteStatus where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverQuote

-- deriving stock instance Ord Domain.DriverQuoteStatus

driverQuoteTMod :: DriverQuoteT (B.FieldModification (B.TableField DriverQuoteT))
driverQuoteTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      searchRequestId = B.fieldNamed "search_request_id",
      searchRequestForDriverId = B.fieldNamed "search_request_for_driver_id",
      driverId = B.fieldNamed "driver_id",
      driverName = B.fieldNamed "driver_name",
      driverRating = B.fieldNamed "driver_rating",
      status = B.fieldNamed "status",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      distance = B.fieldNamed "distance",
      distanceToPickup = B.fieldNamed "distance_to_pickup",
      durationToPickup = B.fieldNamed "duration_to_pickup",
      validTill = B.fieldNamed "valid_till",
      estimatedFare = B.fieldNamed "estimated_fare",
      fareParametersId = B.fieldNamed "fare_parameters_id",
      providerId = B.fieldNamed "provider_id",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverQuoteToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverQuoteToHSModifiers =
  M.fromList
    []

driverQuoteToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverQuoteToPSModifiers =
  M.fromList
    []

defaultDriverQuote :: DriverQuote
defaultDriverQuote =
  DriverQuoteT
    { id = "",
      transactionId = "",
      searchRequestId = "",
      searchRequestForDriverId = Nothing,
      driverId = "",
      driverName = "",
      driverRating = Nothing,
      status = "",
      vehicleVariant = "",
      distance = "",
      distanceToPickup = "",
      durationToPickup = 1.0,
      validTill = defaultDate,
      estimatedFare = "",
      fareParametersId = "",
      providerId = "",
      createdAt = defaultDate,
      updatedAt = defaultDate
    }

instance Serialize DriverQuote where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverQuoteT ['id] [])
