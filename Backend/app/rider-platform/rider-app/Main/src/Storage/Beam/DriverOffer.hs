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

module Storage.Beam.DriverOffer where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverOffer as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data DriverOfferT f = DriverOfferT
  { id :: B.C f Text,
    estimateId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    driverName :: B.C f Text,
    durationToPickup :: B.C f Int,
    distanceToPickup :: B.C f HighPrecMeters,
    validTill :: B.C f Time.UTCTime,
    bppQuoteId :: B.C f Text,
    rating :: B.C f (Maybe Centesimal),
    status :: B.C f Domain.DriverOfferStatus,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverOfferT where
  data PrimaryKey DriverOfferT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverOfferT where
  modelFieldModification = driverOfferTMod
  modelTableName = "driver_offer"
  modelSchemaName = Just "atlas_app"

type DriverOffer = DriverOfferT Identity

instance FromJSON DriverOffer where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverOffer where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverOffer

driverOfferTMod :: DriverOfferT (B.FieldModification (B.TableField DriverOfferT))
driverOfferTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      estimateId = B.fieldNamed "estimate_id",
      merchantId = B.fieldNamed "merchant_id",
      driverName = B.fieldNamed "driver_name",
      durationToPickup = B.fieldNamed "duration_to_pickup",
      distanceToPickup = B.fieldNamed "distance_to_pickup",
      validTill = B.fieldNamed "valid_till",
      bppQuoteId = B.fieldNamed "bpp_quote_id",
      rating = B.fieldNamed "rating",
      status = B.fieldNamed "status",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize DriverOffer where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverOfferToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverOfferToHSModifiers =
  M.empty

driverOfferToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverOfferToPSModifiers =
  M.empty

$(enableKVPG ''DriverOfferT ['id] [['bppQuoteId]])
