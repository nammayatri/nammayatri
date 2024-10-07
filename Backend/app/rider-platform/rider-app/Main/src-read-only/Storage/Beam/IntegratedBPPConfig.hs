{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IntegratedBPPConfig where

import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Base64
import Tools.Beam.UtilsTH

data IntegratedBPPConfigT f = IntegratedBPPConfigT
  { domain :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    qrGeneratedBy :: B.C f BecknV2.FRFS.Enums.Network,
    qrGenerationKey :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Base64.Base64),
    qrVerificationKey :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Base64.Base64),
    qrVerifiedBy :: B.C f BecknV2.FRFS.Enums.Network,
    vehicleCategory :: B.C f BecknV2.OnDemand.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IntegratedBPPConfigT where
  data PrimaryKey IntegratedBPPConfigT f = IntegratedBPPConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IntegratedBPPConfigId . id

type IntegratedBPPConfig = IntegratedBPPConfigT Identity

$(enableKVPG ''IntegratedBPPConfigT ['id] [])

$(mkTableInstances ''IntegratedBPPConfigT "integrated_bpp_config")
