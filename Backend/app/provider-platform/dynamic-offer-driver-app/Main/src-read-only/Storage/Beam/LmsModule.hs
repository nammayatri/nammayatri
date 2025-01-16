{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsModule where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.LmsModule
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.DriverCoins.Types
import Tools.Beam.UtilsTH

data LmsModuleT f = LmsModuleT
  { bonusCoinEventFunction :: B.C f (Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType),
    category :: B.C f Domain.Types.LmsModule.LmsCategory,
    certificationEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    duration :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    languagesAvailableForQuiz :: B.C f [Kernel.External.Types.Language],
    languagesAvailableForVideos :: B.C f [Kernel.External.Types.Language],
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    moduleCompletionCriteria :: B.C f Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleExpiryConfig :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    moduleNameForCertificate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    moduleSection :: B.C f (Kernel.Prelude.Maybe Domain.Types.LmsModule.ModuleSection),
    noOfVideos :: B.C f Kernel.Prelude.Int,
    rank :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    variant :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsModuleT where
  data PrimaryKey LmsModuleT f = LmsModuleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LmsModuleId . id

type LmsModule = LmsModuleT Identity

$(enableKVPG ''LmsModuleT ['id] [])

$(mkTableInstances ''LmsModuleT "lms_module")
