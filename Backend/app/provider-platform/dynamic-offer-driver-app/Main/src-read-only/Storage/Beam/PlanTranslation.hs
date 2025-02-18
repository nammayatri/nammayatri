{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PlanTranslation where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
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
  data PrimaryKey PlanTranslationT f = PlanTranslationId (B.C f Kernel.External.Types.Language) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = PlanTranslationId <$> language <*> planId

type PlanTranslation = PlanTranslationT Identity

$(enableKVPG ''PlanTranslationT ['language, 'planId] [])

$(mkTableInstances ''PlanTranslationT "plan_translation")
