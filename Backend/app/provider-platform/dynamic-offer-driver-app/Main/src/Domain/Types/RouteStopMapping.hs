{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteStopMapping where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data RouteStopMapping = RouteStopMapping
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema, Ord)
