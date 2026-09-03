{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IncentiveJourney where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data IncentiveJourney = IncentiveJourney
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverTag :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    endDate :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyType :: Kernel.Prelude.Maybe Domain.Types.IncentiveJourney.IncentiveJourneyType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType,
    startDate :: Kernel.Prelude.UTCTime,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data IncentiveJourneyType = Daily | Weekly deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''IncentiveJourneyType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''IncentiveJourneyType)
