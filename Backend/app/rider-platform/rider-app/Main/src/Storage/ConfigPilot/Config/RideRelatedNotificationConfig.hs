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

data RideRelatedNotificationConfigDimensions = RideRelatedNotificationConfigDimensions
  { merchantOperatingCityId :: Text,
    timeDiffEvent :: Maybe Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RideRelatedNotificationConfigRider where
  type DimensionsFor 'RideRelatedNotificationConfigRider = RideRelatedNotificationConfigDimensions
  configTypeValue = RideRelatedNotificationConfigRider
  sConfigType = SRideRelatedNotificationConfigRider

instance ConfigDimensions RideRelatedNotificationConfigDimensions where
  type ConfigTypeOf RideRelatedNotificationConfigDimensions = 'RideRelatedNotificationConfigRider
  type ConfigValueTypeOf RideRelatedNotificationConfigDimensions = [DT.RideRelatedNotificationConfig]
  getConfigType _ = RideRelatedNotificationConfigRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG RideRelatedNotificationConfigRider)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
      [ LCP.DimMatcher (.timeDiffEvent) (Just . (.timeDiffEvent)) (==)
      ]
      Nothing
