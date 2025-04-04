{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Types.ConfigPilot where

import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import Data.OpenApi as OpenApi hiding (description, name, tags, version)
import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Version (DeviceType (..))
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data PlatformType = TypeScript | PureScript
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''PlatformType)

deriving instance Enum DeviceType

deriving instance Bounded DeviceType

data ConfigType
  = DriverPoolConfig
  | TransporterConfig
  | RiderConfig
  | FRFSConfig
  | PayoutConfig
  | MerchantServiceUsageConfig
  | HotSpotConfig
  | MerchantConfig
  | RideRelatedNotificationConfig
  | MerchantMessage
  | MerchantPushNotification
  | DriverIntelligentPoolConfig
  | LeaderBoardConfig
  | CoinsConfig
  | DocumentVerificationConfig
  | FleetOwnerDocumentVerificationConfig
  | GoHomeConfig
  | SubscriptionConfig
  | Overlay
  | FarePolicy
  | FareProduct
  | Plan
  | PlanTranslation
  | Toll
  | CancellationFarePolicy
  | SurgePricing
  | MultiModalConfigs
  | UiConfig DeviceType PlatformType
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

class ConfigTypeEnumerable a where
  allValuesConfigTypes :: [a]

instance ConfigTypeEnumerable ConfigType where
  allValuesConfigTypes =
    [ DriverPoolConfig,
      TransporterConfig,
      RiderConfig,
      FRFSConfig,
      PayoutConfig,
      MerchantServiceUsageConfig,
      HotSpotConfig,
      MerchantConfig,
      RideRelatedNotificationConfig,
      MerchantMessage,
      MerchantPushNotification,
      DriverIntelligentPoolConfig,
      LeaderBoardConfig,
      CoinsConfig,
      DocumentVerificationConfig,
      FleetOwnerDocumentVerificationConfig,
      GoHomeConfig,
      SubscriptionConfig,
      Overlay,
      FarePolicy,
      FareProduct,
      Plan,
      PlanTranslation,
      Toll,
      CancellationFarePolicy,
      SurgePricing
    ]
      ++ (UiConfig <$> [minBound .. maxBound] <*> [minBound .. maxBound])

$(mkHttpInstancesForEnum ''ConfigType)
$(mkBeamInstancesForEnum ''ConfigType)

generateConfigTypeShowInstances :: [String]
generateConfigTypeShowInstances =
  map show (Lib.Yudhishthira.Types.ConfigPilot.allValuesConfigTypes :: [ConfigType])

instance ToParamSchema ConfigType where
  toParamSchema _ =
    mempty
      & title ?~ "ConfigType"
      & type_ ?~ OpenApiString
      & enum_ ?~ map (String . T.pack) generateConfigTypeShowInstances

data Config a = Config
  { config :: a,
    extraDimensions :: Maybe Value,
    identifier :: Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON, Show, Ord)

data Person
