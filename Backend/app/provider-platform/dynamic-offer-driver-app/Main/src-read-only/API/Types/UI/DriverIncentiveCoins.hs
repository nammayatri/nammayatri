{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverIncentiveCoins where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.DriverCoins.Types
import Servant
import Tools.Auth

data DriverIncentiveCoinConfigItem = DriverIncentiveCoinConfigItem
  { active :: Kernel.Prelude.Bool,
    coins :: Kernel.Prelude.Int,
    eventFunction :: Lib.DriverCoins.Types.DriverCoinsFunctionType,
    eventName :: Data.Text.Text,
    expirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Coins.CoinsConfig.CoinsConfig,
    ridesThreshold :: Kernel.Prelude.Int,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    tripCategoryType :: Kernel.Prelude.Maybe Lib.DriverCoins.Types.TripCategoryType,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverIncentiveRideCountRes = DriverIncentiveRideCountRes {dayValidRideCount :: Kernel.Prelude.Int, progressValidRideCount :: Kernel.Prelude.Int, timeBoundValidRideCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
