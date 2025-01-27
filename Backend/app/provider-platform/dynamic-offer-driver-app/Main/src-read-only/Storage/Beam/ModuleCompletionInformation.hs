{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ModuleCompletionInformation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ModuleCompletionInformation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ModuleCompletionInformationT f = ModuleCompletionInformationT
  { attempt :: B.C f Kernel.Prelude.Int,
    completionId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    entity :: B.C f Domain.Types.ModuleCompletionInformation.ModuleEntity,
    entityId :: B.C f Kernel.Prelude.Text,
    entityStatus :: B.C f Domain.Types.ModuleCompletionInformation.EntityStatus,
    selectedEntityId :: B.C f [Kernel.Prelude.Text],
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ModuleCompletionInformationT where
  data PrimaryKey ModuleCompletionInformationT f
    = ModuleCompletionInformationId (B.C f Kernel.Prelude.Int) (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.ModuleCompletionInformation.ModuleEntity) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = ModuleCompletionInformationId <$> attempt <*> completionId <*> entity <*> entityId

type ModuleCompletionInformation = ModuleCompletionInformationT Identity

$(enableKVPG ''ModuleCompletionInformationT ['attempt, 'completionId, 'entity, 'entityId] [])

$(mkTableInstances ''ModuleCompletionInformationT "module_completion_information")
