{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Types.MonetaryReward.MonetaryRewardConfig where

import Domain.Types.VehicleCategory as DTV
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Common as KTC
import Kernel.Types.Id
import qualified Lib.DriverCoins.Types as DCT

data MonetaryRewardConfig = MonetaryRewardConfig
  { id :: Id MonetaryRewardConfig,
    eventFunction :: DCT.DriverCoinsFunctionType,
    eventName :: Text,
    merchantId :: Text,
    merchantOptCityId :: Text,
    monetaryRewardAmount :: KTC.HighPrecMoney,
    expirationAt :: Maybe Int,
    active :: Bool,
    vehicleCategory :: Maybe DTV.VehicleCategory
  }
  deriving (Generic, Show, FromJSON, ToJSON)
