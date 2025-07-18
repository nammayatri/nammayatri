{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Types.ConfigPilot where

import Data.Aeson
import Data.OpenApi as OpenApi hiding (description, name, tags, version)
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
  | VehicleServiceTier
  | Toll
  | CancellationFarePolicy
  | SurgePricing
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded, ToParamSchema)

$(mkHttpInstancesForEnum ''ConfigType)
$(mkBeamInstancesForEnum ''ConfigType)

data Config a = Config
  { config :: a,
    extraDimensions :: Maybe Value,
    identifier :: Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON, Show, Ord)

data Person

data UiDevicePlatformReq = UiDevicePlatformReq
  { deviceType :: DeviceType,
    platformType :: PlatformType
  }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, Read, Show)
