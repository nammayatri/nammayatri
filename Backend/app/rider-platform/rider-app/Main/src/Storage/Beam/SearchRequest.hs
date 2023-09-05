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

module Storage.Beam.SearchRequest where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Maps (Language)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

data SearchRequestT f = SearchRequestT
  { id :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    riderId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f (Maybe Text),
    distance :: B.C f (Maybe Centesimal),
    maxDistance :: B.C f (Maybe Centesimal),
    estimatedRideDuration :: B.C f (Maybe Seconds),
    device :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    language :: B.C f (Maybe Language),
    disabilityTag :: B.C f (Maybe Text),
    customerExtraFee :: B.C f (Maybe Money),
    availablePaymentMethods :: B.C f [Text],
    selectedPaymentMethodId :: B.C f (Maybe Text),
    autoAssignEnabled :: B.C f (Maybe Bool),
    autoAssignEnabledV2 :: B.C f (Maybe Bool),
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequest = SearchRequestT Identity

searchRequestTMod :: SearchRequestT (B.FieldModification (B.TableField SearchRequestT))
searchRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      riderId = B.fieldNamed "rider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      distance = B.fieldNamed "distance",
      maxDistance = B.fieldNamed "max_distance",
      estimatedRideDuration = B.fieldNamed "estimated_ride_duration",
      device = B.fieldNamed "device",
      merchantId = B.fieldNamed "merchant_id",
      language = B.fieldNamed "language",
      disabilityTag = B.fieldNamed "disability_tag",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      availablePaymentMethods = B.fieldNamed "available_payment_methods",
      selectedPaymentMethodId = B.fieldNamed "selected_payment_method_id",
      autoAssignEnabled = B.fieldNamed "auto_assign_enabled",
      autoAssignEnabledV2 = B.fieldNamed "auto_assign_enabled_v2",
      bundleVersion = B.fieldNamed "bundle_version",
      clientVersion = B.fieldNamed "client_version",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''SearchRequestT ['id] [['riderId]])

$(mkTableInstances ''SearchRequestT "search_request" "atlas_app")
