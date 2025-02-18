{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.QuestionModuleMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.DriverCoins.Types
import Tools.Beam.UtilsTH

data QuestionModuleMappingT f = QuestionModuleMappingT
  { moduleId :: B.C f Kernel.Prelude.Text,
    questionId :: B.C f Kernel.Prelude.Text,
    quizCoinFunction :: B.C f (Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuestionModuleMappingT where
  data PrimaryKey QuestionModuleMappingT f = QuestionModuleMappingId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = QuestionModuleMappingId <$> moduleId <*> questionId

type QuestionModuleMapping = QuestionModuleMappingT Identity

$(enableKVPG ''QuestionModuleMappingT ['moduleId, 'questionId] [])

$(mkTableInstances ''QuestionModuleMappingT "question_module_mapping")
