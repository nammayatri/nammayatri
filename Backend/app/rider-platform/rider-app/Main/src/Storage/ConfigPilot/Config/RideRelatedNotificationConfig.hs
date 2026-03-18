{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.RideRelatedNotificationConfig
  ( RideRelatedNotificationConfigDimensions (..),
  )
where

import qualified Domain.Types.RideRelatedNotificationConfig as DRRN
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRN
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

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
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey RideRelatedNotificationConfig mocId) 3600 $ do
      cfgs <- SCRRN.findAllByMerchantOperatingCityId (Id mocId) (Just [])
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
      mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG RideRelatedNotificationConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\tde -> c.timeDiffEvent == tde) dims.timeDiffEvent
