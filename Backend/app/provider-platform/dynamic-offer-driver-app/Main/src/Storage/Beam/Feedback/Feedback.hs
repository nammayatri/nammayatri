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

module Storage.Beam.Feedback.Feedback where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Feedback.Feedback as Domain ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data FeedbackT f = FeedbackT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    rideId :: B.C f Text,
    badge :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackT where
  data PrimaryKey FeedbackT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FeedbackT where
  modelFieldModification = feedbackTMod
  modelTableName = "feedback"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Feedback = FeedbackT Identity

instance FromJSON Feedback where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Feedback where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Feedback

feedbackTMod :: FeedbackT (B.FieldModification (B.TableField FeedbackT))
feedbackTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      rideId = B.fieldNamed "ride_id",
      badge = B.fieldNamed "badge",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize Feedback where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

feedbackToHSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackToHSModifiers =
  M.empty

feedbackToPSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackToPSModifiers =
  M.empty

$(enableKVPG ''FeedbackT ['id] [])
