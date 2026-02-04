{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.DriverPoolConfig
  ( DriverPoolDimensions (..),
  )
where

import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Types

data DriverPoolDimensions = DriverPoolDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    txnId :: Maybe Text,
    tripDistance :: Int,
    vehicleVariant :: Text,
    tripCategory :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'DriverPoolConfig where
  type DimensionsFor 'DriverPoolConfig = DriverPoolDimensions
  configTypeValue = DriverPoolConfig
  sConfigType = SDriverPoolConfig

instance ConfigTypeInfo 'DriverIntelligentPoolConfig where
  type DimensionsFor 'DriverIntelligentPoolConfig = DriverPoolDimensions
  configTypeValue = DriverIntelligentPoolConfig
  sConfigType = SDriverIntelligentPoolConfig

instance ConfigDimensions DriverPoolDimensions where
  type ConfigTypeOf DriverPoolDimensions = 'DriverPoolConfig
  type ConfigValueTypeOf DriverPoolDimensions = LYT.TableDataResp
  getConfigType _ = DriverPoolConfig
