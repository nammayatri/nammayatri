{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Merchant where

import Data.Aeson
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
  { id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    subscriberId :: Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    name :: Kernel.Prelude.Text,
    defaultCity :: Kernel.Types.Beckn.Context.City,
    defaultState :: Kernel.Types.Beckn.Context.IndianState,
    country :: Kernel.Types.Beckn.Context.Country,
    geofencingConfig :: Kernel.Types.Geofencing.GeofencingConfig,
    gatewayUrl :: Kernel.Types.Common.BaseUrl,
    registryUrl :: Kernel.Types.Common.BaseUrl,
    fallbackShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    bapId :: Kernel.Prelude.Text,
    bapUniqueKeyId :: Kernel.Prelude.Text,
    driverOfferBaseUrl :: Kernel.Types.Common.BaseUrl,
    driverOfferApiKey :: Kernel.Prelude.Text,
    driverOfferMerchantId :: Kernel.Prelude.Text,
    geoHashPrecisionValue :: Kernel.Prelude.Int,
    minimumDriverRatesCount :: Kernel.Prelude.Int,
    signingPublicKey :: Kernel.Types.Base64.Base64,
    cipherText :: Kernel.Prelude.Maybe Kernel.Types.Base64.Base64,
    signatureExpiry :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    isAvoidToll :: Kernel.Prelude.Bool,
    aadhaarVerificationTryLimit :: Kernel.Prelude.Int,
    aadhaarKeyExpiryTime :: Kernel.Types.Common.Seconds,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    editPickupDistanceThreshold :: Kernel.Types.Common.Distance,
    driverDistanceThresholdFromPickup :: Kernel.Types.Common.Distance,
    numOfAllowedEditPickupLocationAttemptsThreshold :: Kernel.Prelude.Int,
    publicMediaFileUrlPattern :: Kernel.Prelude.Text,
    scheduleRideBufferTime :: Kernel.Prelude.NominalDiffTime,
    fakeOtpMobileNumbers :: [Kernel.Prelude.Text],
    fakeOtpEmails :: [Kernel.Prelude.Text],
    kaptureDisposition :: Kernel.Prelude.Text,
    arrivedPickupThreshold :: Kernel.Types.Common.Distance,
    arrivingPickupThreshold :: Kernel.Types.Common.Distance,
    driverOnTheWayNotifyExpiry :: Kernel.Types.Common.Seconds
  }
  deriving (Generic, Show)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)

instance FromJSON (MerchantD 'Safe)

instance ToJSON (MerchantD 'Safe)
