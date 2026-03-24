{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.InterCityDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Utils.Common
import qualified Database.Beam as B



data InterCityDetailsT f
    = InterCityDetailsT {baseFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         currency :: (B.C f Kernel.Utils.Common.Currency),
                         deadKmFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         id :: (B.C f Kernel.Prelude.Text),
                         distanceUnit :: (B.C f Kernel.Types.Common.DistanceUnit),
                         kmPerPlannedExtraHour :: (B.C f Kernel.Types.Common.HighPrecDistance),
                         nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                         nightShiftEnd :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
                         nightShiftStart :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
                         perDayMaxAllowanceInMins :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes)),
                         perDayMaxHourAllowance :: (B.C f Kernel.Types.Common.Hours),
                         perExtraKmRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         perExtraMinRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         perHourCharge :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         plannedPerKmRateOneWay :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         plannedPerKmRateRoundTrip :: (B.C f Kernel.Types.Common.HighPrecMoney),
                         roundTrip :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                         createdAt :: (B.C f Kernel.Prelude.UTCTime),
                         updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table InterCityDetailsT
    where data PrimaryKey InterCityDetailsT f = InterCityDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = InterCityDetailsId . id
type InterCityDetails = InterCityDetailsT Identity

$(enableKVPG (''InterCityDetailsT) [('id)] [])

$(mkTableInstances (''InterCityDetailsT) "inter_city_details")

