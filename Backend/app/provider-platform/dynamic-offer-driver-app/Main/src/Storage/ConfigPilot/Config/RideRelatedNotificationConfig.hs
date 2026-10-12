{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.RideRelatedNotificationConfig (RideRelatedNotificationConfigDimensions (..)) where

import qualified Domain.Types.RideRelatedNotificationConfig
import qualified Domain.Types.RideRelatedNotificationConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SQ
import qualified Storage.Queries.RideRelatedNotificationConfig as QRRNC

data RideRelatedNotificationConfigDimensions = RideRelatedNotificationConfigDimensions
  { merchantOperatingCityId :: Text,
    timeDiffEvent :: Maybe Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RideRelatedNotificationConfig where
  type DimensionsFor 'RideRelatedNotificationConfig = RideRelatedNotificationConfigDimensions
  configTypeValue = RideRelatedNotificationConfig
  sConfigType = SRideRelatedNotificationConfig

instance ConfigDimensions RideRelatedNotificationConfigDimensions where
  type ConfigTypeOf RideRelatedNotificationConfigDimensions = 'RideRelatedNotificationConfig
  type ConfigValueTypeOf RideRelatedNotificationConfigDimensions = [DT.RideRelatedNotificationConfig]
  getConfigType _ = RideRelatedNotificationConfig
  configDBFetch a = Just (QRRNC.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG RideRelatedNotificationConfig)
      (Id a.merchantOperatingCityId)
      (QRRNC.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.timeDiffEvent) (Just . (.timeDiffEvent)) (==)
      ]
      Nothing
  configFallback a = Just $ case a.timeDiffEvent of
    Just e -> QRRNC.findAllByMerchantOperatingCityIdAndTimeDiffEvent (Id a.merchantOperatingCityId) e
    Nothing -> QRRNC.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId)
