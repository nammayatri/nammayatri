{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.CommonConfig
  ( CommonDimensions (..),
  )
where

import Kernel.Prelude
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Types

-- | Shared dimension type for configs that use common dimensions.
-- Every dimension record includes merchantOperatingCityId and merchantId for fetching configs.
data CommonDimensions = CommonDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    txnId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- -----------------------------------------------------------------------------
-- ConfigTypeInfo instances for all configs using CommonDimensions
-- -----------------------------------------------------------------------------

instance ConfigTypeInfo 'FRFSConfig where
  type DimensionsFor 'FRFSConfig = CommonDimensions
  configTypeValue = FRFSConfig
  sConfigType = SFRFSConfig

instance ConfigTypeInfo 'MerchantServiceUsageConfig where
  type DimensionsFor 'MerchantServiceUsageConfig = CommonDimensions
  configTypeValue = MerchantServiceUsageConfig
  sConfigType = SMerchantServiceUsageConfig

instance ConfigTypeInfo 'HotSpotConfig where
  type DimensionsFor 'HotSpotConfig = CommonDimensions
  configTypeValue = HotSpotConfig
  sConfigType = SHotSpotConfig

instance ConfigTypeInfo 'MerchantConfig where
  type DimensionsFor 'MerchantConfig = CommonDimensions
  configTypeValue = MerchantConfig
  sConfigType = SMerchantConfig

instance ConfigTypeInfo 'RideRelatedNotificationConfig where
  type DimensionsFor 'RideRelatedNotificationConfig = CommonDimensions
  configTypeValue = RideRelatedNotificationConfig
  sConfigType = SRideRelatedNotificationConfig

instance ConfigTypeInfo 'MerchantMessage where
  type DimensionsFor 'MerchantMessage = CommonDimensions
  configTypeValue = MerchantMessage
  sConfigType = SMerchantMessage

instance ConfigTypeInfo 'MerchantPushNotification where
  type DimensionsFor 'MerchantPushNotification = CommonDimensions
  configTypeValue = MerchantPushNotification
  sConfigType = SMerchantPushNotification

instance ConfigTypeInfo 'LeaderBoardConfig where
  type DimensionsFor 'LeaderBoardConfig = CommonDimensions
  configTypeValue = LeaderBoardConfig
  sConfigType = SLeaderBoardConfig

instance ConfigTypeInfo 'CoinsConfig where
  type DimensionsFor 'CoinsConfig = CommonDimensions
  configTypeValue = CoinsConfig
  sConfigType = SCoinsConfig

instance ConfigTypeInfo 'DocumentVerificationConfig where
  type DimensionsFor 'DocumentVerificationConfig = CommonDimensions
  configTypeValue = DocumentVerificationConfig
  sConfigType = SDocumentVerificationConfig

instance ConfigTypeInfo 'FleetOwnerDocumentVerificationConfig where
  type DimensionsFor 'FleetOwnerDocumentVerificationConfig = CommonDimensions
  configTypeValue = FleetOwnerDocumentVerificationConfig
  sConfigType = SFleetOwnerDocumentVerificationConfig

instance ConfigTypeInfo 'GoHomeConfig where
  type DimensionsFor 'GoHomeConfig = CommonDimensions
  configTypeValue = GoHomeConfig
  sConfigType = SGoHomeConfig

instance ConfigTypeInfo 'SubscriptionConfig where
  type DimensionsFor 'SubscriptionConfig = CommonDimensions
  configTypeValue = SubscriptionConfig
  sConfigType = SSubscriptionConfig

instance ConfigTypeInfo 'Overlay where
  type DimensionsFor 'Overlay = CommonDimensions
  configTypeValue = Overlay
  sConfigType = SOverlay

instance ConfigTypeInfo 'FarePolicy where
  type DimensionsFor 'FarePolicy = CommonDimensions
  configTypeValue = FarePolicy
  sConfigType = SFarePolicy

instance ConfigTypeInfo 'FareProduct where
  type DimensionsFor 'FareProduct = CommonDimensions
  configTypeValue = FareProduct
  sConfigType = SFareProduct

instance ConfigTypeInfo 'Plan where
  type DimensionsFor 'Plan = CommonDimensions
  configTypeValue = Plan
  sConfigType = SPlan

instance ConfigTypeInfo 'PlanTranslation where
  type DimensionsFor 'PlanTranslation = CommonDimensions
  configTypeValue = PlanTranslation
  sConfigType = SPlanTranslation

instance ConfigTypeInfo 'VehicleServiceTier where
  type DimensionsFor 'VehicleServiceTier = CommonDimensions
  configTypeValue = VehicleServiceTier
  sConfigType = SVehicleServiceTier

instance ConfigTypeInfo 'Toll where
  type DimensionsFor 'Toll = CommonDimensions
  configTypeValue = Toll
  sConfigType = SToll

instance ConfigTypeInfo 'CancellationFarePolicy where
  type DimensionsFor 'CancellationFarePolicy = CommonDimensions
  configTypeValue = CancellationFarePolicy
  sConfigType = SCancellationFarePolicy

instance ConfigTypeInfo 'SurgePricing where
  type DimensionsFor 'SurgePricing = CommonDimensions
  configTypeValue = SurgePricing
  sConfigType = SSurgePricing
