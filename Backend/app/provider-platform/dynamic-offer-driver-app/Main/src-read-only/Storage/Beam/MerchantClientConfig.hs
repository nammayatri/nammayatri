{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MerchantClientConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import qualified Data.Aeson
import qualified Domain.Types.MerchantClientConfig
import qualified Database.Beam as B



data MerchantClientConfigT f
    = MerchantClientConfigT {clientOS :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType)),
                             configJSON :: (B.C f Data.Aeson.Value),
                             serviceName :: (B.C f Domain.Types.MerchantClientConfig.ClientService),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             packageId :: (B.C f Kernel.Prelude.Text),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                             merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                             merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table MerchantClientConfigT
    where data PrimaryKey MerchantClientConfigT f = MerchantClientConfigId (B.C f Domain.Types.MerchantClientConfig.ClientService) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MerchantClientConfigId <$> serviceName <*> packageId
type MerchantClientConfig = MerchantClientConfigT Identity

$(enableKVPG (''MerchantClientConfigT) [('serviceName), ('packageId)] [])

$(mkTableInstances (''MerchantClientConfigT) "merchant_client_config")

