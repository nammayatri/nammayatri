{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib.Yudhishthira.Types.ConfigPilot where

import Data.Aeson
import Data.OpenApi as OpenApi hiding (description, name, tags, version)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

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
