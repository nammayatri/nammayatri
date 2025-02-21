{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IntegratedBPPConfig where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.IntegratedBPPConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IntegratedBPPConfigT f = IntegratedBPPConfigT
  { domain :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    platformType :: B.C f Domain.Types.IntegratedBPPConfig.PlatformType,
    configJSON :: B.C f Data.Aeson.Value,
    vehicleCategory :: B.C f BecknV2.OnDemand.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IntegratedBPPConfigT where
  data PrimaryKey IntegratedBPPConfigT f
    = IntegratedBPPConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f BecknV2.OnDemand.Enums.VehicleCategory)
    deriving (Generic, B.Beamable)
  primaryKey = IntegratedBPPConfigId <$> domain <*> id <*> merchantId <*> merchantOperatingCityId <*> vehicleCategory

type IntegratedBPPConfig = IntegratedBPPConfigT Identity

$(enableKVPG ''IntegratedBPPConfigT ['domain, 'id, 'merchantId, 'merchantOperatingCityId, 'vehicleCategory] [])

$(mkTableInstances ''IntegratedBPPConfigT "integrated_bpp_config")
