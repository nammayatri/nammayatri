{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Rating where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RatingT f = RatingT
  { id :: B.C f Kernel.Prelude.Text,
    rideId :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f Kernel.Prelude.Text,
    ratingValue :: B.C f Kernel.Prelude.Int,
    feedbackDetails :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    wasOfferedAssistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RatingT where
  data PrimaryKey RatingT f = RatingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RatingId . id

type Rating = RatingT Identity

$(enableKVPG ''RatingT ['id] [['rideId]])

$(mkTableInstances ''RatingT "rating")
