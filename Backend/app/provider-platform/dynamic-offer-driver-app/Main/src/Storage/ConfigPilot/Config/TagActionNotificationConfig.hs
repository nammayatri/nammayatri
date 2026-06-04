{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.TagActionNotificationConfig (TagActionNotificationConfigDimensions (..)) where

import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Storage.Queries.TagActionNotificationConfig as SQ
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Lib.Yudhishthira.Types.TagActionNotificationConfig as DT
import Storage.Beam.Yudhishthira ()

data TagActionNotificationConfigDimensions = TagActionNotificationConfigDimensions
  { merchantOperatingCityId :: Text,
    notificationKey :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'TagActionNotificationConfig where
  type DimensionsFor 'TagActionNotificationConfig = TagActionNotificationConfigDimensions
  configTypeValue = TagActionNotificationConfig
  sConfigType = STagActionNotificationConfig

instance ConfigDimensions TagActionNotificationConfigDimensions where
  type ConfigTypeOf TagActionNotificationConfigDimensions = 'TagActionNotificationConfig
  type ConfigValueTypeOf TagActionNotificationConfigDimensions = [DT.TagActionNotificationConfig]
  getConfigType _ = TagActionNotificationConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG TagActionNotificationConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.notificationKey) (Just . (.notificationKey)) (==)
      ]
      Nothing
