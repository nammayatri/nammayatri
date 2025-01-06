{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Rating where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RatingT f = RatingT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Data.Text.Text,
    feedbackDetails :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    id :: B.C f Data.Text.Text,
    isFavourite :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSafe :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    issueId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    mediaId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    ratingValue :: B.C f Kernel.Prelude.Int,
    rideId :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    wasOfferedAssistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table RatingT where
  data PrimaryKey RatingT f = RatingId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = RatingId . id

type Rating = RatingT Identity

$(enableKVPG ''RatingT ['id] [['rideId]])

$(mkTableInstances ''RatingT "rating")
