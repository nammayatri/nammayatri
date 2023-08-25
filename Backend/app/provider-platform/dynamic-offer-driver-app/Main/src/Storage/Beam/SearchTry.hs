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

module Storage.Beam.SearchTry where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.SearchTry as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

data SearchTryT f = SearchTryT
  { id :: B.C f Text,
    messageId :: B.C f Text,
    requestId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    estimateId :: B.C f Text,
    baseFare :: B.C f Money,
    customerExtraFee :: B.C f (Maybe Money),
    status :: B.C f Domain.SearchTryStatus,
    vehicleVariant :: B.C f Variant.Variant,
    searchRepeatCounter :: B.C f Int,
    searchRepeatType :: B.C f Domain.SearchRepeatType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchTryT where
  data PrimaryKey SearchTryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchTry = SearchTryT Identity

searchTryTMod :: SearchTryT (B.FieldModification (B.TableField SearchTryT))
searchTryTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      messageId = B.fieldNamed "message_id",
      requestId = B.fieldNamed "request_id",
      estimateId = B.fieldNamed "estimate_id",
      merchantId = B.fieldNamed "merchant_id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      baseFare = B.fieldNamed "base_fare",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      status = B.fieldNamed "status",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      searchRepeatCounter = B.fieldNamed "search_repeat_counter",
      searchRepeatType = B.fieldNamed "search_repeat_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''SearchTryT ['id] [['requestId]])

$(mkTableInstances ''SearchTryT "search_try" "atlas_driver_offer_bpp")
