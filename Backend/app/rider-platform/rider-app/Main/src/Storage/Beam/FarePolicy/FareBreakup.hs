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

module Storage.Beam.FarePolicy.FareBreakup where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data FareBreakupT f = FareBreakupT
  { id :: B.C f Text,
    bookingId :: B.C f Text,
    description :: B.C f Text,
    amount :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table FareBreakupT where
  data PrimaryKey FareBreakupT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FareBreakupT where
  modelFieldModification = fareBreakupTMod
  modelTableName = "fare_breakup"
  modelSchemaName = Just "atlas_app"

type FareBreakup = FareBreakupT Identity

instance FromJSON FareBreakup where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareBreakup where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FareBreakup

fareBreakupTMod :: FareBreakupT (B.FieldModification (B.TableField FareBreakupT))
fareBreakupTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      bookingId = B.fieldNamed "booking_id",
      description = B.fieldNamed "description",
      amount = B.fieldNamed "amount"
    }

instance Serialize FareBreakup where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

fareBreakupToHSModifiers :: M.Map Text (A.Value -> A.Value)
fareBreakupToHSModifiers =
  M.empty

fareBreakupToPSModifiers :: M.Map Text (A.Value -> A.Value)
fareBreakupToPSModifiers =
  M.empty

$(enableKVPG ''FareBreakupT ['id] [['bookingId]])
