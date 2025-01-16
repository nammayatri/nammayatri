{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverProfileQuestions where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverProfileQuestionsT f = DriverProfileQuestionsT
  { aboutMe :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aspirations :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    drivingSince :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    hometown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    imageIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    pledges :: B.C f [Kernel.Prelude.Text],
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleTags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverProfileQuestionsT where
  data PrimaryKey DriverProfileQuestionsT f = DriverProfileQuestionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverProfileQuestionsId . driverId

type DriverProfileQuestions = DriverProfileQuestionsT Identity

$(enableKVPG ''DriverProfileQuestionsT ['driverId] [])

$(mkTableInstances ''DriverProfileQuestionsT "driver_profile_questions")
