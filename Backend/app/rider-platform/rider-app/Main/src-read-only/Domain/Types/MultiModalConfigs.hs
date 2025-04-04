{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalConfigs where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data MultiModalConfigs = MultiModalConfigs
  { busFilterTimeBufferInSeconds :: Kernel.Types.Time.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    enableBusFiltering :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalConfigs.MultiModalConfigs,
    makeMultiModalSearch :: Kernel.Prelude.Bool,
    maxAllowedPublicTransportLegs :: Kernel.Prelude.Int,
    maximumWalkDistance :: Kernel.Types.Common.Meters,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metroBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minimumWalkDistance :: Kernel.Types.Common.Meters,
    multimodalTesting :: Kernel.Prelude.Bool,
    nearbyDriverSearchRadius :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    permissibleModes :: Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType],
    straightLineThreshold :: Kernel.Types.Common.Meters,
    suburbanBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
