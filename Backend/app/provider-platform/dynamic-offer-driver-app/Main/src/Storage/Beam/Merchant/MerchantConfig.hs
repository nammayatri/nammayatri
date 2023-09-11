{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Storage.Beam.Merchant.MerchantConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Geofencing
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data MerchantConfigT f = MerchantConfigT
  { merchantId :: B.C f Text,
    gstin :: B.C f (Maybe Text),
    name :: B.C f Text,
    verified :: B.C f Bool,
    enabled :: B.C f Bool,
    description :: B.C f (Maybe Text),
    mobileNumber :: B.C f (Maybe Text),
    mobileCountryCode :: B.C f (Maybe Text),
    fromTime :: B.C f (Maybe Time.UTCTime),
    toTime :: B.C f (Maybe Time.UTCTime),
    internalApiKey :: B.C f Text,
    headCount :: B.C f (Maybe Int),
    status :: B.C f Domain.Status,
    info :: B.C f (Maybe Text),
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    city :: B.C f Context.City,
    country :: B.C f Context.Country,
    geoHashPrecisionValue :: B.C f Int,
    minimumDriverRatesCount :: B.C f Int,
    pickupLocThreshold :: B.C f Meters,
    dropLocThreshold :: B.C f Meters,
    rideTimeEstimatedThreshold :: B.C f Seconds,
    includeDriverCurrentlyOnRide :: B.C f Bool,
    defaultPopupDelay :: B.C f Seconds,
    popupDelayToAddAsPenalty :: B.C f (Maybe Seconds),
    thresholdCancellationScore :: B.C f (Maybe Int),
    minRidesForCancellationScore :: B.C f (Maybe Int),
    thresholdCancellationPercentageToUnlist :: B.C f (Maybe Int),
    minRidesToUnlist :: B.C f (Maybe Int),
    mediaFileUrlPattern :: B.C f Text,
    mediaFileSizeUpperLimit :: B.C f Int,
    referralLinkPassword :: B.C f Text,
    fcmUrl :: B.C f Text,
    fcmServiceAccount :: B.C f Text,
    fcmTokenKeyPrefix :: B.C f Text,
    onboardingTryLimit :: B.C f Int,
    onboardingRetryTimeInHours :: B.C f Int,
    checkImageExtractionForDashboard :: B.C f Bool,
    searchRepeatLimit :: B.C f Int,
    actualRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    upwardsRecomputeBuffer :: B.C f HighPrecMeters,
    approxRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    driverPaymentCycleBuffer :: B.C f Seconds,
    driverPaymentCycleDuration :: B.C f Seconds,
    driverPaymentCycleStartTime :: B.C f Seconds,
    driverPaymentReminderInterval :: B.C f Seconds,
    driverAutoPayNotificationTime :: B.C f Seconds,
    driverAutoPayExecutionTime :: B.C f Seconds,
    rcLimit :: B.C f Int,
    automaticRCActivationCutOff :: B.C f Seconds,
    timeDiffFromUtc :: B.C f Seconds,
    subscription :: B.C f Bool,
    minLocationAccuracy :: B.C f Double,
    aadhaarVerificationRequired :: B.C f Bool,
    enableDashboardSms :: B.C f Bool,
    subscriptionStartTime :: B.C f UTCTime,
    mandateValidity :: B.C f Int,
    driverLocationAccuracyBuffer :: B.C f Meters,
    routeDeviationThreshold :: B.C f Meters,
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    registryUrl :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantConfig = MerchantConfigT Identity

$(enableKVPG ''MerchantConfigT ['merchantId] [])

$(mkTableInstancesWithTModifier ''MerchantConfigT "merchant_config" [("automaticRCActivationCutOff", "automatic_r_c_activation_cut_off")])
