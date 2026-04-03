{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverPlan where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DriverInformation
import qualified Kernel.Types.Common
import qualified Domain.Types.Plan
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.DriverPlan
import qualified Database.Beam as B



data DriverPlanT f
    = DriverPlanT {autoPayStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus)),
                   coinCovertedToCashLeft :: (B.C f Kernel.Types.Common.HighPrecMoney),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   driverId :: (B.C f Kernel.Prelude.Text),
                   enableServiceUsageCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                   isCategoryLevelSubscriptionEnabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                   isOnFreeTrial :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                   lastBillGeneratedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                   lastPaymentLinkSentAtIstDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                   mandateId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   mandateSetupDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                   merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   merchantOpCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   payerVpa :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   planId :: (B.C f Kernel.Prelude.Text),
                   planType :: (B.C f Domain.Types.Plan.PaymentMode),
                   serviceName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.Plan.ServiceNames)),
                   rentedVehicleNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   totalAmountChargedForService :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                   totalCoinsConvertedCash :: (B.C f Kernel.Types.Common.HighPrecMoney),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                   vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
                   waiveOfMode :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPlan.WaiveOffMode)),
                   waiveOffEnabledOn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                   waiveOffValidTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                   waiverOffPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))}
    deriving (Generic, B.Beamable)
instance B.Table DriverPlanT
    where data PrimaryKey DriverPlanT f = DriverPlanId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverPlanId . driverId
type DriverPlan = DriverPlanT Identity

$(enableKVPG (''DriverPlanT) [('driverId)] [[('mandateId)]])

$(mkTableInstances (''DriverPlanT) "driver_plan")

