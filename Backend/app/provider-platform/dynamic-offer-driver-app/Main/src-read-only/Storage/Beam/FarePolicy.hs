{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicy where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.FarePolicy
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyT f = FarePolicyT
  { airportConvenienceFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    maxAllowedTripDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    minAllowedTripDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    boothCharges :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.BoothCharge)),
    businessDiscountPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    cancellationCommissionChargeConfig :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    cancellationFarePolicyId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    cardChargePerDistanceUnitMultiplier :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    fixedCardCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    cgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    commissionChargeConfig :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    congestionCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.CongestionChargeMultiplier)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    driverAllowance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverCancellationNotAllowed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    farePolicyType :: (B.C f Domain.Types.Extra.FarePolicy.FarePolicyType),
    govtCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    nightShiftEnd :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
    nightShiftStart :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
    parkingCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perDistanceUnitInsuranceCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perLuggageCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perMinuteRideExtraTimeCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    perStopCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    personalDiscountPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    petCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    pickupBufferInSecsForNightShiftCal :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    platformFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    platformFeeChargesBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.PlatformFeeMethods)),
    priorityCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    returnFee :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.ReturnFee)),
    rideExtraTimeChargeGracePeriod :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    schedulingCharge :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.FarePolicy.SchedulingCharge)),
    serviceCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    serviceChargeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    sgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tipOptions :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int])),
    tollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tollTaxChargeConfig :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vatChargeConfig :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f = FarePolicyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyId . id

type FarePolicy = FarePolicyT Identity

$(enableKVPG (''FarePolicyT) [('id)] [])

$(mkTableInstances (''FarePolicyT) "fare_policy")

$(Domain.Types.UtilsTH.mkCacParseInstance (''FarePolicyT))
