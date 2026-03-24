{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MerchantServiceConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Aeson
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.UtilsTH
import qualified Database.Beam as B



data MerchantServiceConfigT f
    = MerchantServiceConfigT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                              configJSON :: (B.C f Data.Aeson.Value),
                              serviceName :: (B.C f Domain.Types.MerchantServiceConfig.ServiceName),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MerchantServiceConfigT
    where data PrimaryKey MerchantServiceConfigT f = MerchantServiceConfigId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.MerchantServiceConfig.ServiceName) deriving (Generic, B.Beamable)
          primaryKey = MerchantServiceConfigId <$> merchantId <*> serviceName
type MerchantServiceConfig = MerchantServiceConfigT Identity

$(enableKVPG (''MerchantServiceConfigT) [('merchantId), ('serviceName)] [])

$(mkTableInstances (''MerchantServiceConfigT) "merchant_service_config")

$(Domain.Types.UtilsTH.mkCacParseInstance (''MerchantServiceConfigT))

