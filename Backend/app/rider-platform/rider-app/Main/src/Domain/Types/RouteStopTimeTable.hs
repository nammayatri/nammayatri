{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteStopTimeTable where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data RouteStopTimeTable = RouteStopTimeTable
  { delay :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    platformCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isStageStop :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    routeCode :: Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Domain.Types.RouteStopTimeTable.SourceType,
    stage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    providerStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopCode :: Kernel.Prelude.Text,
    timeOfArrival :: Kernel.Prelude.TimeOfDay,
    timeOfDeparture :: Kernel.Prelude.TimeOfDay,
    tripId :: Kernel.Types.Id.Id Domain.Types.RouteStopTimeTable.RouteStopTimeTable,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SourceType = LIVE | GTFS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SourceType))
