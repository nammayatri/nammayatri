{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleTrip where

import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleTrip = VehicleTrip
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    earnings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    endReason :: Kernel.Prelude.Maybe Domain.Types.VehicleTrip.VehicleTripEndReason,
    endedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Kernel.Prelude.Text,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.VehicleTrip.VehicleTripStatus,
    toStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VehicleTripEndReason = END_ROUTE | ROUTE_CHANGED | END_FOR_NOW | SESSION_TIMEOUT deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data VehicleTripStatus = ACTIVE | COMPLETED | ABANDONED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleTripStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleTripEndReason)
