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

module Storage.Beam.Feedback.FeedbackBadge where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data FeedbackBadgeT f = FeedbackBadgeT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    badge :: B.C f Text,
    badgeCount :: B.C f Int,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackBadgeT where
  data PrimaryKey FeedbackBadgeT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FeedbackBadgeT where
  modelFieldModification = feedbackBadgeTMod
  modelTableName = "feedback_badge"
  modelSchemaName = Just "atlas_app"

type FeedbackBadge = FeedbackBadgeT Identity

instance FromJSON FeedbackBadge where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FeedbackBadge where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FeedbackBadge

feedbackBadgeTMod :: FeedbackBadgeT (B.FieldModification (B.TableField FeedbackBadgeT))
feedbackBadgeTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      badge = B.fieldNamed "badge",
      badgeCount = B.fieldNamed "badge_count",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize FeedbackBadge where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

feedbackBadgeToHSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackBadgeToHSModifiers =
  M.empty

feedbackBadgeToPSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackBadgeToPSModifiers =
  M.empty

$(enableKVPG ''FeedbackBadgeT ['id] [['driverId]])
