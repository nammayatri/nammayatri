{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverModuleCompletion where

import qualified Database.Beam as B
import qualified Domain.Types.DriverModuleCompletion
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data DriverModuleCompletionT f = DriverModuleCompletionT
  { completedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    completionId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    entitiesCompleted :: B.C f [Domain.Types.DriverModuleCompletion.ModuleCompletionEntity],
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    moduleId :: B.C f Kernel.Prelude.Text,
    ratingAtTheTimeOfCompletion :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    startedAt :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.DriverModuleCompletion.ModuleCompletionStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverModuleCompletionT where
  data PrimaryKey DriverModuleCompletionT f = DriverModuleCompletionId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = DriverModuleCompletionId . completionId

type DriverModuleCompletion = DriverModuleCompletionT Identity

$(enableKVPG ''DriverModuleCompletionT ['completionId] [])

$(mkTableInstances ''DriverModuleCompletionT "driver_module_completion")
