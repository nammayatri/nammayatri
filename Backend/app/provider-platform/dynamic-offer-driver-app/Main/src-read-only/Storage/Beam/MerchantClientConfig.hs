{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantClientConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantClientConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data MerchantClientConfigT f = MerchantClientConfigT
  { clientOS :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    configJSON :: B.C f Data.Aeson.Value,
    serviceName :: B.C f Domain.Types.MerchantClientConfig.ClientService,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    packageId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantClientConfigT where
  data PrimaryKey MerchantClientConfigT f = MerchantClientConfigId (B.C f Domain.Types.MerchantClientConfig.ClientService) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantClientConfigId <$> serviceName <*> packageId

type MerchantClientConfig = MerchantClientConfigT Identity

$(enableKVPG ''MerchantClientConfigT ['serviceName, 'packageId] [])

$(mkTableInstances ''MerchantClientConfigT "merchant_client_config")
