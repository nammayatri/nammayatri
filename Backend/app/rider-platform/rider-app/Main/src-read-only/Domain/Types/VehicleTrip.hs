{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleTrip where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleTrip = VehicleTrip
  { capacity :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Text,
    endReason :: Kernel.Prelude.Maybe Domain.Types.VehicleTrip.VehicleTripEndReason,
    endedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    missedPickups :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    movingAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    offlineBoardings :: Kernel.Prelude.Int,
    reachedEndAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    routeCode :: Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.VehicleTrip.VehicleTripStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VehicleTripEndReason
  = END_ROUTE
  | RETURN
  | ROUTE_CHANGED
  | END_FOR_NOW
  | SESSION_TIMEOUT
  | OPS_FORCED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data VehicleTripStatus = ACTIVE | PAUSED | COMPLETED | ABANDONED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleTripStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleTripEndReason)
