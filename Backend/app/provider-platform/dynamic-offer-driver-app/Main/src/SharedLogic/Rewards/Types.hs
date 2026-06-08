{-# OPTIONS_GHC -Wno-unused-imports #-}

module SharedLogic.Rewards.Types
  ( RewardDispatchContext (..),
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTCfg
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT

data RewardDispatchContext = RewardDispatchContext
  { eventType :: DCT.DriverCoinsEventType,
    entityId :: Maybe Text,
    vehCategory :: DTV.VehicleCategory,
    mbServiceTierType :: Maybe DTC.ServiceTierType,
    mbConfigVersionMap :: Maybe [LYT.ConfigVersionMap],
    transporterConfig :: DTCfg.TransporterConfig,
    mbFleetOwnerId :: Maybe (Id DP.Person)
  }
