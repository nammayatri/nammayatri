{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantPushNotification
  ( MerchantPushNotificationDimensions (..),
  )
where

import qualified Domain.Types.MerchantPushNotification as DMPN
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data MerchantPushNotificationDimensions = MerchantPushNotificationDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'MerchantPushNotification where
  type DimensionsFor 'MerchantPushNotification = MerchantPushNotificationDimensions
  configTypeValue = MerchantPushNotification
  sConfigType = SMerchantPushNotification

instance ConfigDimensions MerchantPushNotificationDimensions where
  type ConfigTypeOf MerchantPushNotificationDimensions = 'MerchantPushNotification
  type ConfigValueTypeOf MerchantPushNotificationDimensions = [DMPN.MerchantPushNotification]
  getConfigType _ = MerchantPushNotification
  getConfig a = do
    cfgs <- SQMPN.findAllByMerchantOpCityId (Id (merchantOperatingCityId a))
    let configWrapper = LYT.Config {config = cfgs, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantPushNotification) (Id (merchantOperatingCityId a))
