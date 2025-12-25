{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Merchant where

import qualified Database.Beam as B
import qualified Domain.Types
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Base64
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Geofencing
import Tools.Beam.UtilsTH

data MerchantT f = MerchantT
  { aadhaarKeyExpiryTime :: B.C f Kernel.Types.Common.Seconds,
    aadhaarVerificationTryLimit :: B.C f Kernel.Prelude.Int,
    arrivedPickupThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    arrivedPickupThresholdValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    arrivingPickupThreshold :: B.C f Kernel.Types.Common.HighPrecDistance,
    bapId :: B.C f Kernel.Prelude.Text,
    bapUniqueKeyId :: B.C f Kernel.Prelude.Text,
    cipherText :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Base64.Base64),
    country :: B.C f Kernel.Types.Beckn.Context.Country,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    city :: B.C f Kernel.Types.Beckn.Context.City,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    driverDistanceThresholdFromPickup :: B.C f Kernel.Types.Common.HighPrecMeters,
    driverDistanceThresholdFromPickupValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    driverOfferApiKey :: B.C f Kernel.Prelude.Text,
    driverOfferBaseUrl :: B.C f Kernel.Prelude.Text,
    driverOfferMerchantId :: B.C f Kernel.Prelude.Text,
    driverOnTheWayNotifyExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    editPickupDistanceThreshold :: B.C f Kernel.Types.Common.HighPrecMeters,
    editPickupDistanceThresholdValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    fakeOtpEmails :: B.C f [Kernel.Prelude.Text],
    fakeOtpMobileNumbers :: B.C f [Kernel.Prelude.Text],
    fallbackShortId :: B.C f Kernel.Prelude.Text,
    gatewayAndRegistryPriorityList :: B.C f (Kernel.Prelude.Maybe [Domain.Types.GatewayAndRegistryService]),
    gatewayUrl :: B.C f Kernel.Prelude.Text,
    geoHashPrecisionValue :: B.C f Kernel.Prelude.Int,
    destinationRestriction :: B.C f Kernel.Types.Geofencing.GeoRestriction,
    originRestriction :: B.C f Kernel.Types.Geofencing.GeoRestriction,
    id :: B.C f Kernel.Prelude.Text,
    isAvoidToll :: B.C f Kernel.Prelude.Bool,
    kaptureDisposition :: B.C f Kernel.Prelude.Text,
    mediaFileSizeUpperLimit :: B.C f Kernel.Prelude.Int,
    mediaFileUrlPattern :: B.C f Kernel.Prelude.Text,
    minimumDriverRatesCount :: B.C f Kernel.Prelude.Int,
    name :: B.C f Kernel.Prelude.Text,
    numOfAllowedEditLocationAttemptsThreshold :: B.C f Kernel.Prelude.Int,
    numOfAllowedEditPickupLocationAttemptsThreshold :: B.C f Kernel.Prelude.Int,
    onlinePayment :: B.C f Kernel.Prelude.Bool,
    publicMediaFileUrlPattern :: B.C f Kernel.Prelude.Text,
    registryUrl :: B.C f Kernel.Prelude.Text,
    scheduleRideBufferTime :: B.C f Kernel.Types.Common.Seconds,
    shortId :: B.C f Kernel.Prelude.Text,
    signatureExpiry :: B.C f Kernel.Prelude.Int,
    signingPrivateKey :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Base64.Base64),
    signingPublicKey :: B.C f Kernel.Types.Base64.Base64,
    stuckRideAutoCancellationBuffer :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    subscriberId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f = MerchantId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantId . id

type Merchant = MerchantT Identity

$(enableKVPG ''MerchantT ['id] [['shortId], ['subscriberId]])

$(mkTableInstances ''MerchantT "merchant")
