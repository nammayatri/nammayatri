{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.GoHomeConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data GoHomeConfig = GoHomeConfig
  { activeTime :: Kernel.Prelude.Int,
    addStartWaypointAt :: Kernel.Types.Common.Meters,
    cancellationCnt :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    destRadiusMeters :: Kernel.Prelude.Int,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    enableGoHome :: Kernel.Prelude.Bool,
    goHomeBatchDelay :: Kernel.Types.Common.Seconds,
    goHomeFromLocationRadius :: Kernel.Types.Common.Meters,
    goHomeWayPointRadius :: Kernel.Types.Common.Meters,
    ignoreWaypointsTill :: Kernel.Types.Common.Meters,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    newLocAllowedRadius :: Kernel.Types.Common.Meters,
    numDriversForDirCheck :: Kernel.Prelude.Int,
    numHomeLocations :: Kernel.Prelude.Int,
    startCnt :: Kernel.Prelude.Int,
    updateHomeLocationAfterSec :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Subscriber = Subscriber {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
