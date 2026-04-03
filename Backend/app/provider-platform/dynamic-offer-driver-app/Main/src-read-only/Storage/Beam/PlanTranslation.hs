{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PlanTranslation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Text
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PlanTranslationT f
    = PlanTranslationT {description :: (B.C f Data.Text.Text),
                        language :: (B.C f Kernel.External.Types.Language),
                        name :: (B.C f Data.Text.Text),
                        planId :: (B.C f Data.Text.Text),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PlanTranslationT
    where data PrimaryKey PlanTranslationT f = PlanTranslationId (B.C f Kernel.External.Types.Language) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = PlanTranslationId <$> language <*> planId
type PlanTranslation = PlanTranslationT Identity

$(enableKVPG (''PlanTranslationT) [('language), ('planId)] [])

$(mkTableInstances (''PlanTranslationT) "plan_translation")

