{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..)) where

import qualified Domain.Types.ScheduledPayoutConfig as DT
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.ScheduledPayoutConfig as SQ

data ScheduledPayoutConfigDimensions = ScheduledPayoutConfigDimensions
  { merchantOperatingCityId :: Text,
    isEnabled :: Maybe Kernel.Prelude.Bool,
    payoutCategory :: Maybe Lib.Payment.Domain.Types.Common.EntityName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'ScheduledPayoutConfig where
  type DimensionsFor 'ScheduledPayoutConfig = ScheduledPayoutConfigDimensions
  configTypeValue = ScheduledPayoutConfig
  sConfigType = SScheduledPayoutConfig

instance ConfigDimensions ScheduledPayoutConfigDimensions where
  type ConfigTypeOf ScheduledPayoutConfigDimensions = 'ScheduledPayoutConfig
  type ConfigValueTypeOf ScheduledPayoutConfigDimensions = [DT.ScheduledPayoutConfig]
  getConfigType _ = ScheduledPayoutConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG ScheduledPayoutConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.isEnabled) (Just . (.isEnabled)) (==),
        LCP.DimMatcher (.payoutCategory) (Just . (.payoutCategory)) (==)
      ]
      Nothing
