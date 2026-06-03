{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantPushNotification
  ( MerchantPushNotificationDimensions (..),
  )
where

import qualified Domain.Types.MerchantPushNotification as DMPN
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
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
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG MerchantPushNotification)
      (Id a.merchantOperatingCityId)
      (SQMPN.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      []
      Nothing
