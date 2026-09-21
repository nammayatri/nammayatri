{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AddOnConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.AddOnConfig
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data AddOnConfigT f = AddOnConfigT
  { addOnType :: (B.C f Domain.Types.AddOnConfig.AddOnType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    descriptorName :: (B.C f Kernel.Prelude.Text),
    descriptorShortDesc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    maxQuantity :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    pricePerQuantity :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTier :: (B.C f [Domain.Types.Common.ServiceTierType])
  }
  deriving (Generic, B.Beamable)

instance B.Table AddOnConfigT where
  data PrimaryKey AddOnConfigT f = AddOnConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AddOnConfigId . id

type AddOnConfig = AddOnConfigT Identity

$(enableKVPG (''AddOnConfigT) [('id)] [])

$(mkTableInstances (''AddOnConfigT) "add_on_config")
