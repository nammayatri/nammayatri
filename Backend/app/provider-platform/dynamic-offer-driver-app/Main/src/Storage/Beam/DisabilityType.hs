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

module Storage.Beam.DisabilityType where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Types
import Kernel.Prelude hiding (Generic)
import Sequelize as Se

data DisabilityTypeT f = DisabilityTypeT
  { tag :: B.C f Text,
    language :: B.C f Language,
    onBookingMessage :: B.C f Text,
    onArrivalMessage :: B.C f Text,
    onRideStartMessage :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityTypeT where
  data PrimaryKey DisabilityTypeT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . tag

instance ModelMeta DisabilityTypeT where
  modelFieldModification = disabilityTypeTMod
  modelTableName = "disability_type"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DisabilityType = DisabilityTypeT Identity

instance FromJSON DisabilityType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DisabilityType where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DisabilityType

disabilityTypeTMod :: DisabilityTypeT (B.FieldModification (B.TableField DisabilityTypeT))
disabilityTypeTMod =
  B.tableModification
    { tag = B.fieldNamed "tag",
      language = B.fieldNamed "language",
      onBookingMessage = B.fieldNamed "on_booking_message",
      onArrivalMessage = B.fieldNamed "on_arrival_message",
      onRideStartMessage = B.fieldNamed "on_ride_start_message"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

disabilityTypeToHSModifiers :: M.Map Text (A.Value -> A.Value)
disabilityTypeToHSModifiers =
  M.empty

disabilityTypeToPSModifiers :: M.Map Text (A.Value -> A.Value)
disabilityTypeToPSModifiers =
  M.empty

instance Serialize DisabilityType where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DisabilityTypeT ['tag] [])
