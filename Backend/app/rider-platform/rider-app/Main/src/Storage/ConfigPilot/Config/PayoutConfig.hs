{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.PayoutConfig
  ( PayoutDimensions (..),
  )
where

import qualified Domain.Types.PayoutConfig as DPC
import Kernel.Prelude
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Types

data PayoutDimensions = PayoutDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    txnId :: Maybe Text,
    payoutType :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'PayoutConfig where
  type DimensionsFor 'PayoutConfig = PayoutDimensions
  configTypeValue = PayoutConfig
  sConfigType = SPayoutConfig

instance ConfigDimensions PayoutDimensions where
  type ConfigTypeOf PayoutDimensions = 'PayoutConfig
  type ConfigValueTypeOf PayoutDimensions = [DPC.PayoutConfig]
  getConfigType _ = PayoutConfig
