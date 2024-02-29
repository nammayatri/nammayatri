{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PlanTranslation where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.Plan
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data PlanTranslationT f = PlanTranslationT
  { description :: B.C f Data.Text.Text,
    language :: B.C f Kernel.External.Types.Language,
    name :: B.C f Data.Text.Text,
    planId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PlanTranslationT where
  data PrimaryKey PlanTranslationT f = PlanTranslationId (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PlanTranslationId . planId

type PlanTranslation = PlanTranslationT Identity

$(enableKVPG ''PlanTranslationT ['planId] [['language]])

$(mkTableInstances ''PlanTranslationT "plan_translation")
