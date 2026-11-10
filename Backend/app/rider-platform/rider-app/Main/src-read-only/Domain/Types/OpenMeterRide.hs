{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OpenMeterRide where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data OpenMeterRide = OpenMeterRide
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    distanceMeters :: Kernel.Types.Common.Meters,
    encodedPolyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endReason :: Kernel.Prelude.Maybe Domain.Types.OpenMeterRide.OpenMeterEndReason,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedGapMeters :: Kernel.Types.Common.Meters,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide,
    lastHeartbeatAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pointsCount :: Kernel.Prelude.Int,
    serverLinearDistanceMeters :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    serverStartedAt :: Kernel.Prelude.UTCTime,
    startLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.OpenMeterRide.OpenMeterRideStatus,
    tariffSnapshot :: Kernel.Prelude.Maybe Data.Aeson.Value,
    trackingConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime,
    waitingTimeSeconds :: Kernel.Types.Common.Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data OpenMeterEndReason
  = USER
  | SYSTEM_PAUSED
  | CONFLICT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data OpenMeterRideStatus
  = RUNNING
  | ENDED
  | CANCELLED
  | AUTO_ENDED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OpenMeterEndReason)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OpenMeterRideStatus)

$(mkHttpInstancesForEnum ''OpenMeterRideStatus)
