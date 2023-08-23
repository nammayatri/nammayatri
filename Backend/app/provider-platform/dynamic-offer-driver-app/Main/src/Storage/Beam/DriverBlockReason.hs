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

module Storage.Beam.DriverBlockReason where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data DriverBlockReasonT f = DriverBlockReasonT
  { reasonCode :: B.C f Text,
    blockReason :: B.C f (Maybe Text),
    blockTimeInHours :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBlockReasonT where
  data PrimaryKey DriverBlockReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . reasonCode

instance ModelMeta DriverBlockReasonT where
  modelFieldModification = driverBlockReasonTMod
  modelTableName = "driver_block_reason"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverBlockReason = DriverBlockReasonT Identity

instance FromJSON DriverBlockReason where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverBlockReason where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverBlockReason

driverBlockReasonTMod :: DriverBlockReasonT (B.FieldModification (B.TableField DriverBlockReasonT))
driverBlockReasonTMod =
  B.tableModification
    { reasonCode = B.fieldNamed "reason_code",
      blockReason = B.fieldNamed "block_reason",
      blockTimeInHours = B.fieldNamed "block_time_in_hours"
    }

instance Serialize DriverBlockReason where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverBlockReasonToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverBlockReasonToHSModifiers =
  M.empty

driverBlockReasonToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverBlockReasonToPSModifiers =
  M.empty

$(enableKVPG ''DriverBlockReasonT ['reasonCode] [])
