{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Merchant where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Base64
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Geofencing
import Tools.Beam.UtilsTH

data MerchantT f = MerchantT
  { id :: B.C f Kernel.Prelude.Text,
    subscriberId :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    city :: B.C f Kernel.Types.Beckn.Context.City,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    country :: B.C f Kernel.Types.Beckn.Context.Country,
    destinationRestriction :: B.C f Kernel.Types.Geofencing.GeoRestriction,
    originRestriction :: B.C f Kernel.Types.Geofencing.GeoRestriction,
    gatewayUrl :: B.C f Kernel.Prelude.Text,
    registryUrl :: B.C f Kernel.Prelude.Text,
    fallbackShortId :: B.C f Kernel.Prelude.Text,
    bapId :: B.C f Kernel.Prelude.Text,
    bapUniqueKeyId :: B.C f Kernel.Prelude.Text,
    driverOfferBaseUrl :: B.C f Kernel.Prelude.Text,
    driverOfferApiKey :: B.C f Kernel.Prelude.Text,
    driverOfferMerchantId :: B.C f Kernel.Prelude.Text,
    geoHashPrecisionValue :: B.C f Kernel.Prelude.Int,
    minimumDriverRatesCount :: B.C f Kernel.Prelude.Int,
    signingPublicKey :: B.C f Kernel.Types.Base64.Base64,
    cipherText :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Base64.Base64),
    signatureExpiry :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    isAvoidToll :: B.C f Kernel.Prelude.Bool,
    aadhaarVerificationTryLimit :: B.C f Kernel.Prelude.Int,
    aadhaarKeyExpiryTime :: B.C f Kernel.Types.Common.Seconds,
    mediaFileSizeUpperLimit :: B.C f Kernel.Prelude.Int,
    mediaFileUrlPattern :: B.C f Kernel.Prelude.Text,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    editPickupDistanceThreshold :: B.C f Kernel.Types.Common.HighPrecMeters,
    editPickupDistanceThresholdValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    driverDistanceThresholdFromPickup :: B.C f Kernel.Types.Common.HighPrecMeters,
    driverDistanceThresholdFromPickupValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    numOfAllowedEditPickupLocationAttemptsThreshold :: B.C f Kernel.Prelude.Int,
    publicMediaFileUrlPattern :: B.C f Kernel.Prelude.Text,
    scheduleRideBufferTime :: B.C f Kernel.Types.Common.Seconds,
    fakeOtpMobileNumbers :: B.C f [Kernel.Prelude.Text],
    fakeOtpEmails :: B.C f [Kernel.Prelude.Text],
    kaptureDisposition :: B.C f Kernel.Prelude.Text,
    arrivedPickupThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    arrivedPickupThresholdValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    arrivingPickupThreshold :: B.C f Kernel.Types.Common.HighPrecDistance,
    driverOnTheWayNotifyExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f = MerchantId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantId . id

type Merchant = MerchantT Identity

$(enableKVPG ''MerchantT ['id] [['subscriberId], ['shortId]])

$(mkTableInstances ''MerchantT "merchant")
