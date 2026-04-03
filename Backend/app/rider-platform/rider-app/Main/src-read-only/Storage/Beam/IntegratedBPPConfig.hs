{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.IntegratedBPPConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.IntegratedBPPConfig
import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B



data IntegratedBPPConfigT f
    = IntegratedBPPConfigT {agencyKey :: (B.C f Kernel.Prelude.Text),
                            domain :: (B.C f Kernel.Prelude.Text),
                            feedKey :: (B.C f Kernel.Prelude.Text),
                            id :: (B.C f Kernel.Prelude.Text),
                            isTicketValidOnMultipleRoutes :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                            merchantId :: (B.C f Kernel.Prelude.Text),
                            merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                            platformType :: (B.C f Domain.Types.IntegratedBPPConfig.PlatformType),
                            configJSON :: (B.C f Data.Aeson.Value),
                            providerName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            vehicleCategory :: (B.C f BecknV2.OnDemand.Enums.VehicleCategory),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table IntegratedBPPConfigT
    where data PrimaryKey IntegratedBPPConfigT f = IntegratedBPPConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = IntegratedBPPConfigId . id
type IntegratedBPPConfig = IntegratedBPPConfigT Identity

$(enableKVPG (''IntegratedBPPConfigT) [('id)] [[('agencyKey)]])

$(mkTableInstances (''IntegratedBPPConfigT) "integrated_bpp_config")

