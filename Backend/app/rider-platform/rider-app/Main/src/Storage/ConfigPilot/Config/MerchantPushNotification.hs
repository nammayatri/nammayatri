{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.MerchantPushNotification (MerchantPushNotificationDimensions (..)) where

import qualified Domain.Types.MerchantPushNotification as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantPushNotification as SQ

data MerchantPushNotificationDimensions = MerchantPushNotificationDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantPushNotificationRider where
  type DimensionsFor 'MerchantPushNotificationRider = MerchantPushNotificationDimensions
  configTypeValue = MerchantPushNotificationRider
  sConfigType = SMerchantPushNotificationRider

instance ConfigDimensions MerchantPushNotificationDimensions where
  type ConfigTypeOf MerchantPushNotificationDimensions = 'MerchantPushNotificationRider
  type ConfigValueTypeOf MerchantPushNotificationDimensions = [DT.MerchantPushNotification]
  getConfigType _ = MerchantPushNotificationRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG MerchantPushNotification)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      ([] :: [LCP.DimMatcher MerchantPushNotificationDimensions DT.MerchantPushNotification])
      Nothing
