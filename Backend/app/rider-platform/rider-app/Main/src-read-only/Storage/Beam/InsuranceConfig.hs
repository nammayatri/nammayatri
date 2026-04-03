{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.InsuranceConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data InsuranceConfigT f
    = InsuranceConfigT {allowedVehicleServiceTiers :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType])),
                        city :: (B.C f Kernel.Prelude.Text),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        driverInsuredAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        hours :: (B.C f Kernel.Prelude.Int),
                        id :: (B.C f Kernel.Prelude.Text),
                        insuredAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        merchantId :: (B.C f Kernel.Prelude.Text),
                        merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                        partnerId :: (B.C f Kernel.Prelude.Text),
                        plan :: (B.C f Kernel.Prelude.Text),
                        planType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                        state :: (B.C f Kernel.Prelude.Text),
                        tripCategory :: (B.C f Domain.Types.Common.TripCategory),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                        vehicleCategory :: (B.C f Domain.Types.VehicleCategory.VehicleCategory)}
    deriving (Generic, B.Beamable)
instance B.Table InsuranceConfigT
    where data PrimaryKey InsuranceConfigT f = InsuranceConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = InsuranceConfigId . id
type InsuranceConfig = InsuranceConfigT Identity

$(enableKVPG (''InsuranceConfigT) [('id)] [])

$(mkTableInstances (''InsuranceConfigT) "insurance_config")

