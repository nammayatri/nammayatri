{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverProfileQuestions where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverProfileQuestionsT f = DriverProfileQuestionsT
  { aspirations :: B.C f [Kernel.Prelude.Text],
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    expertAt :: B.C f [Kernel.Prelude.Text],
    hometown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    pledges :: B.C f [Kernel.Prelude.Text],
    whyNY :: B.C f [Kernel.Prelude.Text]
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverProfileQuestionsT where
  data PrimaryKey DriverProfileQuestionsT f = DriverProfileQuestionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverProfileQuestionsId . driverId

type DriverProfileQuestions = DriverProfileQuestionsT Identity

$(enableKVPG ''DriverProfileQuestionsT ['driverId] [])

$(mkTableInstances ''DriverProfileQuestionsT "driver_profile_questions")
