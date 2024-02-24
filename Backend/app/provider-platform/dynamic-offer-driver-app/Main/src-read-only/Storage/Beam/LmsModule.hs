{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsModule where

import qualified Database.Beam as B
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data LmsModuleT f = LmsModuleT
  { category :: B.C f Domain.Types.LmsModule.LmsCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    duration :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    languagesAvailableForQuiz :: B.C f [Kernel.External.Types.Language],
    languagesAvailableForVideos :: B.C f [Kernel.External.Types.Language],
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    moduleCompletionCriteria :: B.C f Domain.Types.LmsModule.ModuleCompletionCriteria,
    noOfVideos :: B.C f Kernel.Prelude.Int,
    rank :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    variant :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant.Variant),
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsModuleT where
  data PrimaryKey LmsModuleT f = LmsModuleId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = LmsModuleId . id

type LmsModule = LmsModuleT Identity

$(enableKVPG ''LmsModuleT ['id] [])

$(mkTableInstances ''LmsModuleT "lms_module")
