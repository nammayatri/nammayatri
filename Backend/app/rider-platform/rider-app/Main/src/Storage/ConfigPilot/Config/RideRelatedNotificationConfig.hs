{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.RideRelatedNotificationConfig
  ( RideRelatedNotificationConfigDimensions (..),
  )
where

import qualified Domain.Types.RideRelatedNotificationConfig as DRRN
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRN

data RideRelatedNotificationConfigDimensions = RideRelatedNotificationConfigDimensions
  { merchantOperatingCityId :: Text,
    timeDiffEvent :: Maybe DRRN.TimeDiffEvent
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RideRelatedNotificationConfig where
  type DimensionsFor 'RideRelatedNotificationConfig = RideRelatedNotificationConfigDimensions
  configTypeValue = RideRelatedNotificationConfig
  sConfigType = SRideRelatedNotificationConfig

instance ConfigDimensions RideRelatedNotificationConfigDimensions where
  type ConfigTypeOf RideRelatedNotificationConfigDimensions = 'RideRelatedNotificationConfig
  type ConfigValueTypeOf RideRelatedNotificationConfigDimensions = [DRRN.RideRelatedNotificationConfig]
  getConfigType _ = RideRelatedNotificationConfig
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG RideRelatedNotificationConfig)
      (Id a.merchantOperatingCityId)
      (SCRRN.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
      [CR.DimMatcher (.timeDiffEvent) (Just . (.timeDiffEvent)) (==)]
      Nothing
