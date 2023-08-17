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

module Storage.Beam.PlanTranslation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance IsString Language where
  fromString = show

data PlanTranslationT f = PlanTranslationT
  { planId :: B.C f Text,
    language :: B.C f Language,
    name :: B.C f Text,
    description :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PlanTranslationT where
  data PrimaryKey PlanTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . planId

instance ModelMeta PlanTranslationT where
  modelFieldModification = planTranslationTMod
  modelTableName = "plan_translation"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type PlanTranslation = PlanTranslationT Identity

instance FromJSON PlanTranslation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON PlanTranslation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show PlanTranslation

planTranslationTMod :: PlanTranslationT (B.FieldModification (B.TableField PlanTranslationT))
planTranslationTMod =
  B.tableModification
    { planId = B.fieldNamed "plan_id",
      language = B.fieldNamed "language",
      name = B.fieldNamed "name",
      description = B.fieldNamed "description"
    }

instance Serialize PlanTranslation where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

planTranslationToHSModifiers :: M.Map Text (A.Value -> A.Value)
planTranslationToHSModifiers =
  M.empty

planTranslationToPSModifiers :: M.Map Text (A.Value -> A.Value)
planTranslationToPSModifiers =
  M.empty

$(enableKVPG ''PlanTranslationT ['planId] [['language]])
