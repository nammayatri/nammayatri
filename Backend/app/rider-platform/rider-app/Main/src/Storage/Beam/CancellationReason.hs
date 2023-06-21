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

module Storage.Beam.CancellationReason where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data CancellationReasonT f = CancellationReasonT
  { reasonCode :: B.C f Text,
    description :: B.C f Text,
    enabled :: B.C f Bool,
    onSearch :: B.C f Bool,
    onConfirm :: B.C f Bool,
    onAssign :: B.C f Bool,
    priority :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationReasonT where
  data PrimaryKey CancellationReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . reasonCode

instance ModelMeta CancellationReasonT where
  modelFieldModification = cancellationReasonTMod
  modelTableName = "cancellation_reason"
  modelSchemaName = Just "atlas_app"

type CancellationReason = CancellationReasonT Identity

instance FromJSON CancellationReason where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON CancellationReason where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show CancellationReason

cancellationReasonTMod :: CancellationReasonT (B.FieldModification (B.TableField CancellationReasonT))
cancellationReasonTMod =
  B.tableModification
    { reasonCode = B.fieldNamed "reason_code",
      description = B.fieldNamed "description",
      enabled = B.fieldNamed "enabled",
      onSearch = B.fieldNamed "on_search",
      onConfirm = B.fieldNamed "on_confirm",
      onAssign = B.fieldNamed "on_assign",
      priority = B.fieldNamed "priority"
    }

defaultCancellationReason :: CancellationReason
defaultCancellationReason =
  CancellationReasonT
    { reasonCode = "",
      description = "",
      enabled = False,
      onSearch = False,
      onConfirm = False,
      onAssign = False,
      priority = 0
    }

instance Serialize CancellationReason where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

cancellationReasonToHSModifiers :: M.Map Text (A.Value -> A.Value)
cancellationReasonToHSModifiers =
  M.empty

cancellationReasonToPSModifiers :: M.Map Text (A.Value -> A.Value)
cancellationReasonToPSModifiers =
  M.empty

$(enableKVPG ''CancellationReasonT ['reasonCode] [])
