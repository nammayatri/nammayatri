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
import Tools.Beam.UtilsTH

data QuoteT f = QuoteT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    discount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceToNearestDriver :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    distanceToNearestDriverValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    driverOfferId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fareProductType :: (B.C f Domain.Types.FarePolicy.FareProductType.FareProductType),
    rentalDetailsId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    specialZoneQuoteId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    estimatedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    estimatedTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    itemId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    providerId :: (B.C f Kernel.Prelude.Text),
    providerUrl :: (B.C f Kernel.Prelude.Text),
    requestId :: (B.C f Kernel.Prelude.Text),
    serviceTierName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    serviceTierShortDesc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    specialLocationTag :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    tripTermsId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTierType :: (B.C f Domain.Types.VehicleServiceTier.VehicleServiceTierType)
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteT where
  data PrimaryKey QuoteT f = QuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = QuoteId . id

type Quote = QuoteT Identity

$(enableKVPG (''QuoteT) [('id)] [[('driverOfferId)], [('requestId)]])

$(mkTableInstances (''QuoteT) "quote")
