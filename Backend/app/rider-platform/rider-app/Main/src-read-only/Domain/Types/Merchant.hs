{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Merchant where

import Data.Aeson
import qualified Domain.Types
import Domain.Types.Common (UsageSafety (..))
import Kernel.Prelude
import qualified Kernel.Types.Base64
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Geofencing
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import qualified Tools.Beam.UtilsTH

data MerchantD (s :: UsageSafety) = Merchant
  { aadhaarKeyExpiryTime :: Kernel.Types.Common.Seconds,
    aadhaarVerificationTryLimit :: Kernel.Prelude.Int,
    arrivedPickupThreshold :: Kernel.Types.Common.Distance,
    arrivingPickupThreshold :: Kernel.Types.Common.Distance,
    bapId :: Kernel.Prelude.Text,
    bapUniqueKeyId :: Kernel.Prelude.Text,
    cipherText :: Kernel.Prelude.Maybe Kernel.Types.Base64.Base64,
    country :: Kernel.Types.Beckn.Context.Country,
    createdAt :: Kernel.Prelude.UTCTime,
    defaultCity :: Kernel.Types.Beckn.Context.City,
    defaultState :: Kernel.Types.Beckn.Context.IndianState,
    driverDistanceThresholdFromPickup :: Kernel.Types.Common.Distance,
    driverOfferApiKey :: Kernel.Prelude.Text,
    driverOfferBaseUrl :: Kernel.Types.Common.BaseUrl,
    driverOfferMerchantId :: Kernel.Prelude.Text,
    driverOnTheWayNotifyExpiry :: Kernel.Types.Common.Seconds,
    editPickupDistanceThreshold :: Kernel.Types.Common.Distance,
    fakeOtpEmails :: [Kernel.Prelude.Text],
    fakeOtpMobileNumbers :: [Kernel.Prelude.Text],
    fallbackShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    gatewayAndRegistryPriorityList :: [Domain.Types.GatewayAndRegistryService],
    gatewayUrl :: Kernel.Types.Common.BaseUrl,
    geoHashPrecisionValue :: Kernel.Prelude.Int,
    geofencingConfig :: Kernel.Types.Geofencing.GeofencingConfig,
    id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    isAvoidToll :: Kernel.Prelude.Bool,
    kaptureDisposition :: Kernel.Prelude.Text,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    minimumDriverRatesCount :: Kernel.Prelude.Int,
    name :: Kernel.Prelude.Text,
    numOfAllowedEditLocationAttemptsThreshold :: Kernel.Prelude.Int,
    numOfAllowedEditPickupLocationAttemptsThreshold :: Kernel.Prelude.Int,
    onlinePayment :: Kernel.Prelude.Bool,
    publicMediaFileUrlPattern :: Kernel.Prelude.Text,
    registryUrl :: Kernel.Types.Common.BaseUrl,
    scheduleRideBufferTime :: Kernel.Prelude.NominalDiffTime,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    signatureExpiry :: Kernel.Prelude.Int,
    signingPrivateKey :: Kernel.Prelude.Maybe Kernel.Types.Base64.Base64,
    signingPublicKey :: Kernel.Types.Base64.Base64,
    stuckRideAutoCancellationBuffer :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    subscriberId :: Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)

instance FromJSON (MerchantD 'Safe)

instance ToJSON (MerchantD 'Safe)
