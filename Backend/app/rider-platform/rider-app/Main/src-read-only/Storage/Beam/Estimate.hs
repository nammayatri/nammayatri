{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Estimate where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.EstimateStatus
import qualified Domain.Types.ServiceTierType
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data EstimateT f = EstimateT
  { backendAppVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    backendConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    boostSearchPreSelectionServiceTierConfig :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType])),
    bppEstimateId :: (B.C f Kernel.Prelude.Text),
    businessDiscount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    businessDiscountPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    clientBundleVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientManufacturer :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientModelName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientOsType :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType)),
    clientOsVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientSdkVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    device :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    discount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    driversLocation :: (B.C f [Kernel.External.Maps.LatLong]),
    estimateTags :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    estimatedDistanceValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    estimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    estimatedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    estimatedPickupDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    estimatedStaticDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    estimatedTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    insuredAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    isAirConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isBlockedRoute :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isCustomerPrefferedSearchRoute :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isInsured :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isMultimodalSearch :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    itemId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    nightShiftChargeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    nightShiftEnd :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
    nightShiftStart :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
    oldNightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal)),
    personalDiscount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    personalDiscountPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    providerCompletedRidesCount :: (B.C f Kernel.Prelude.Int),
    providerId :: (B.C f Kernel.Prelude.Text),
    providerMobileNumber :: (B.C f Kernel.Prelude.Text),
    providerName :: (B.C f Kernel.Prelude.Text),
    providerUrl :: (B.C f Kernel.Prelude.Text),
    qar :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    requestId :: (B.C f Kernel.Prelude.Text),
    serviceTierName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    serviceTierShortDesc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    smartTipReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    smartTipSuggestion :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    specialLocationName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    specialLocationTag :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Domain.Types.EstimateStatus.EstimateStatus),
    tipOptions :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int])),
    tollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    maxTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    minTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tripCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory)),
    tripTermsId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory)),
    vehicleIconUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleServiceTierAirConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    vehicleServiceTierSeatingCapacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    vehicleVariant :: (B.C f Domain.Types.ServiceTierType.ServiceTierType),
    waitingChargePerMin :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    waitingChargePerMinAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f = EstimateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EstimateId . id

type Estimate = EstimateT Identity

$(enableKVPG (''EstimateT) [('id)] [[('bppEstimateId)], [('requestId)]])

$(mkTableInstancesWithTModifier (''EstimateT) "estimate" [("oldNightShiftCharge", "night_shift_multiplier")])
