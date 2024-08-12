{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Quote where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.VehicleServiceTier
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data QuoteT f = QuoteT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    discount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    distanceToNearestDriver :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    distanceToNearestDriverValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    driverOfferId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fareProductType :: B.C f Domain.Types.FarePolicy.FareProductType.FareProductType,
    rentalDetailsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialZoneQuoteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    estimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    estimatedPickupDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedTotalFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    id :: B.C f Kernel.Prelude.Text,
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isBlockedRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isCustomerPrefferedSearchRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    itemId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    providerUrl :: B.C f Kernel.Prelude.Text,
    requestId :: B.C f Kernel.Prelude.Text,
    serviceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceTierShortDesc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    tripTermsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleModel :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleServiceTierAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleServiceTierSeatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleVariant :: B.C f Domain.Types.VehicleServiceTier.VehicleServiceTierType
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteT where
  data PrimaryKey QuoteT f = QuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = QuoteId . id

type Quote = QuoteT Identity

$(enableKVPG ''QuoteT ['id] [['driverOfferId], ['requestId]])

$(mkTableInstances ''QuoteT "quote")
