{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantServiceConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantServiceConfigT f = MerchantServiceConfigT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    configJSON :: B.C f Data.Aeson.Value,
    serviceName :: B.C f Domain.Types.MerchantServiceConfig.ServiceName,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceConfigT where
  data PrimaryKey MerchantServiceConfigT f = MerchantServiceConfigId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.MerchantServiceConfig.ServiceName) deriving (Generic, B.Beamable)
  primaryKey = MerchantServiceConfigId <$> merchantId <*> serviceName

type MerchantServiceConfig = MerchantServiceConfigT Identity

$(enableKVPG ''MerchantServiceConfigT ['merchantId, 'serviceName] [])

$(mkTableInstances ''MerchantServiceConfigT "merchant_service_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''MerchantServiceConfigT)
