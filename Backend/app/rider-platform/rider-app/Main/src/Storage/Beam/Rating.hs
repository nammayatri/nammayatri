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

module Storage.Beam.Rating where

import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH (enableKVPG, mkTableInstances)

data RatingT f = RatingT
  { id :: B.C f Text,
    rideId :: B.C f Text,
    riderId :: B.C f Text,
    ratingValue :: B.C f Int,
    feedbackDetails :: B.C f (Maybe Text),
    wasOfferedAssistance :: B.C f (Maybe Bool),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RatingT where
  data PrimaryKey RatingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Rating = RatingT Identity

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

ratingToHSModifiers :: M.Map Text (A.Value -> A.Value)
ratingToHSModifiers =
  M.empty

ratingToPSModifiers :: M.Map Text (A.Value -> A.Value)
ratingToPSModifiers =
  M.empty

$(enableKVPG ''RatingT ['id] [['rideId]])

$(mkTableInstances ''RatingT "rating")
