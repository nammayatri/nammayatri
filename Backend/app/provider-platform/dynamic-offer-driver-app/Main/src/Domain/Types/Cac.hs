{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Cac where

import qualified Domain.Types.DriverPoolConfig as DPC
import qualified Domain.Types.FarePolicy as FP
import qualified Domain.Types.GoHomeConfig as GHC
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as DIPC
import qualified Domain.Types.Merchant.TransporterConfig as TC
import EulerHS.Prelude
import EulerHS.Types (OptionEntity)

data GoHomeConfig = GoHomeConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data TransporterConfig = TransporterConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data DriverPoolConfig = DriverPoolConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data DriverIntelligentPoolConfig = DriverIntelligentPoolConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data FarePolicy = FarePolicy
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity GoHomeConfig GHC.GoHomeConfig

instance OptionEntity TransporterConfig TC.TransporterConfig

instance OptionEntity DriverPoolConfig DPC.DriverPoolConfig

instance OptionEntity DriverIntelligentPoolConfig DIPC.DriverIntelligentPoolConfig

instance OptionEntity FarePolicy FP.FarePolicy
