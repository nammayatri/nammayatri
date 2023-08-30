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

module Storage.Beam.GoHomeConfig where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Meters)
import Lib.Utils ()
import Sequelize

data GoHomeConfigT f = GoHomeConfigT
  { merchantId :: B.C f Text,
    enableGoHome :: B.C f Bool,
    startCnt :: B.C f Int,
    destRadiusMeters :: B.C f Int,
    activeTime :: B.C f Int,
    updateHomeLocationAfterSec :: B.C f Int,
    cancellationCnt :: B.C f Int,
    numHomeLocations :: B.C f Int,
    goHomeFromLocationRadius :: B.C f Meters,
    goHomeWayPointRadius :: B.C f Meters,
    numDriversForDirCheck :: B.C f Int,
    goHomeBatchDelay :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table GoHomeConfigT where
  data PrimaryKey GoHomeConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta GoHomeConfigT where
  modelFieldModification = goHomeConfigTMod
  modelTableName = "go_home_config"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type GoHomeConfig = GoHomeConfigT Identity

instance FromJSON GoHomeConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON GoHomeConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show GoHomeConfig

goHomeConfigTMod :: GoHomeConfigT (B.FieldModification (B.TableField GoHomeConfigT))
goHomeConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      enableGoHome = B.fieldNamed "enable_go_home",
      startCnt = B.fieldNamed "start_cnt",
      destRadiusMeters = B.fieldNamed "dest_radius",
      activeTime = B.fieldNamed "active_time",
      updateHomeLocationAfterSec = B.fieldNamed "update_home_location_after_sec",
      cancellationCnt = B.fieldNamed "cancecllation_cnt",
      numHomeLocations = B.fieldNamed "num_home_locations",
      goHomeFromLocationRadius = B.fieldNamed "go_home_from_location_radius",
      goHomeWayPointRadius = B.fieldNamed "go_home_way_point_radius",
      numDriversForDirCheck = B.fieldNamed "num_drivers_for_dir_check",
      goHomeBatchDelay = B.fieldNamed "go_home_batch_delay",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize GoHomeConfig where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

goHomeConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
goHomeConfigToHSModifiers =
  M.empty

goHomeConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
goHomeConfigToPSModifiers =
  M.empty

$(enableKVPG ''GoHomeConfigT ['merchantId] [])
