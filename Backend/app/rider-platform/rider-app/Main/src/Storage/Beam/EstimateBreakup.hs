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

module Storage.Beam.EstimateBreakup where

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

data EstimateBreakupT f = EstimateBreakupT
  { id :: B.C f Text,
    estimateId :: B.C f Text,
    title :: B.C f Text,
    priceCurrency :: B.C f Text,
    priceValue :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateBreakupT where
  data PrimaryKey EstimateBreakupT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta EstimateBreakupT where
  modelFieldModification = estimateBreakupTMod
  modelTableName = "estimate_breakup"
  modelSchemaName = Just "atlas_app"

type EstimateBreakup = EstimateBreakupT Identity

instance FromJSON EstimateBreakup where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON EstimateBreakup where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show EstimateBreakup

estimateBreakupTMod :: EstimateBreakupT (B.FieldModification (B.TableField EstimateBreakupT))
estimateBreakupTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      estimateId = B.fieldNamed "estimate_id",
      title = B.fieldNamed "title",
      priceCurrency = B.fieldNamed "price_currency",
      priceValue = B.fieldNamed "price_value"
    }

instance Serialize EstimateBreakup where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

estimateBreakupToHSModifiers :: M.Map Text (A.Value -> A.Value)
estimateBreakupToHSModifiers =
  M.empty

estimateBreakupToPSModifiers :: M.Map Text (A.Value -> A.Value)
estimateBreakupToPSModifiers =
  M.empty

$(enableKVPG ''EstimateBreakupT ['id] [['estimateId]])
