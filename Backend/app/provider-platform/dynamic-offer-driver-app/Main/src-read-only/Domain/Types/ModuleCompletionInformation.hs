{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ModuleCompletionInformation where

import Data.Aeson
import qualified Domain.Types.DriverModuleCompletion
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ModuleCompletionInformation = ModuleCompletionInformation
  { attempt :: Kernel.Prelude.Int,
    completionId :: Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion,
    createdAt :: Kernel.Prelude.UTCTime,
    entity :: Domain.Types.ModuleCompletionInformation.ModuleEntity,
    entityId :: Kernel.Prelude.Text,
    entityStatus :: Domain.Types.ModuleCompletionInformation.EntityStatus,
    selectedEntityId :: [Kernel.Prelude.Text],
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityStatus = ENTITY_PASSED | ENTITY_FAILED | ENTITY_ONGOING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ModuleEntity = QUIZ | VIDEO deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EntityStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleEntity)
