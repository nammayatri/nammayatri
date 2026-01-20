{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverQuote where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.DriverQuote
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data DriverQuoteT f = DriverQuoteT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    coinsRewardedOnGoldTierRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    commissionCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    createdAt :: B.C f Data.Time.LocalTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    distanceToPickup :: B.C f Kernel.Types.Common.Meters,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverId :: B.C f Kernel.Prelude.Text,
    driverName :: B.C f Kernel.Prelude.Text,
    driverRating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    durationToPickup :: B.C f Kernel.Types.Common.Seconds,
    estimateId :: B.C f Kernel.Prelude.Text,
    estimatedFare :: B.C f Kernel.Types.Common.Money,
    estimatedFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fareParametersId :: B.C f Kernel.Prelude.Text,
    goHomeRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    reactBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestId :: B.C f Kernel.Prelude.Text,
    searchRequestForDriverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    searchTryId :: B.C f Kernel.Prelude.Text,
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.DriverQuote.DriverQuoteStatus,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f Data.Time.LocalTime,
    validTill :: B.C f Data.Time.LocalTime,
    vehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType),
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleVariant :: B.C f Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverQuoteT where
  data PrimaryKey DriverQuoteT f = DriverQuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverQuoteId . id

type DriverQuote = DriverQuoteT Identity

$(enableKVPG ''DriverQuoteT ['id] [['driverId], ['requestId], ['searchTryId]])

$(mkTableInstancesWithTModifier ''DriverQuoteT "driver_quote" [("requestId", "search_request_id")])
