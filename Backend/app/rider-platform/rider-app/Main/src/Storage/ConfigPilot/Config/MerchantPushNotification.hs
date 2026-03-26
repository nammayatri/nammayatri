{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantPushNotification
  ( MerchantPushNotificationDimensions (..),
  )
where

import qualified Domain.Types.MerchantPushNotification as DMPN
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.MerchantPushNotification as SQMPN

data MerchantPushNotificationDimensions = MerchantPushNotificationDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantPushNotification where
  type DimensionsFor 'MerchantPushNotification = MerchantPushNotificationDimensions
  configTypeValue = MerchantPushNotification
  sConfigType = SMerchantPushNotification

instance ConfigDimensions MerchantPushNotificationDimensions where
  type ConfigTypeOf MerchantPushNotificationDimensions = 'MerchantPushNotification
  type ConfigValueTypeOf MerchantPushNotificationDimensions = [DMPN.MerchantPushNotification]
  getConfigType _ = MerchantPushNotification
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    cfgs <- IM.withInMemCache (configPilotInMemKey MerchantPushNotification mocId) 3600 $ SQMPN.findAllByMerchantOpCityId (Id mocId)
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantPushNotification) (Id mocId)) configWrappers
