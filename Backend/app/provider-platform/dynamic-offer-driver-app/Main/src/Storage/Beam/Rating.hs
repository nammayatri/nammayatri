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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Rating where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
-- import qualified Database.Beam.Schema.Tables as BST
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data RatingT f = RatingT
  { id :: B.C f Text,
    rideId :: B.C f Text,
    driverId :: B.C f Text,
    ratingValue :: B.C f Int,
    feedbackDetails :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RatingT where
  data PrimaryKey RatingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Rating = RatingT Identity

-- ratingTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RatingT)
-- ratingTable =
--   BST.setEntitySchema (Just "atlas_driver_offer_bpp")
--     <> B.setEntityName "rating"
--     <> B.modifyTableFields ratingTMod

-- instance FromJSON Rating where
--   parseJSON = A.genericParseJSON A.defaultOptions

-- instance ToJSON Rating where
--   toJSON = A.genericToJSON A.defaultOptions

-- deriving stock instance Show Rating

ratingTMod :: RatingT (B.FieldModification (B.TableField RatingT))
ratingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      rideId = B.fieldNamed "ride_id",
      driverId = B.fieldNamed "driver_id",
      ratingValue = B.fieldNamed "rating_value",
      feedbackDetails = B.fieldNamed "feedback_details",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''RatingT ['id] [['rideId], ['driverId]])

$(mkTableInstances ''RatingT "rating" "atlas_driver_offer_bpp")
