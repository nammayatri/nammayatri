{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PurchasedPass where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified BecknV2.FRFS.Enums
import qualified Kernel.Prelude
import qualified Domain.Types.PurchasedPass
import qualified Kernel.Types.Common
import qualified Data.Time.Calendar
import qualified Database.Beam as B



data PurchasedPassT f
    = PurchasedPassT {applicableVehicleServiceTiers :: (B.C f [BecknV2.FRFS.Enums.ServiceTierType]),
                      benefitDescription :: (B.C f Kernel.Prelude.Text),
                      benefitType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.PurchasedPass.BenefitType)),
                      benefitValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                      deviceId :: (B.C f Kernel.Prelude.Text),
                      deviceSwitchCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      endDate :: (B.C f Data.Time.Calendar.Day),
                      id :: (B.C f Kernel.Prelude.Text),
                      maxValidDays :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      maxValidTrips :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      merchantId :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      passAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                      passCode :: (B.C f Kernel.Prelude.Text),
                      passDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      passName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      passNumber :: (B.C f Kernel.Prelude.Int),
                      passTypeId :: (B.C f Kernel.Prelude.Text),
                      personId :: (B.C f Kernel.Prelude.Text),
                      preferredDestination :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      preferredSource :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      profilePicture :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      startDate :: (B.C f Data.Time.Calendar.Day),
                      status :: (B.C f Domain.Types.PurchasedPass.StatusType),
                      usedTripCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      verificationValidity :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PurchasedPassT
    where data PrimaryKey PurchasedPassT f = PurchasedPassId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PurchasedPassId . id
type PurchasedPass = PurchasedPassT Identity

$(enableKVPG (''PurchasedPassT) [('id)] [[('passNumber)]])

$(mkTableInstances (''PurchasedPassT) "purchased_pass")

