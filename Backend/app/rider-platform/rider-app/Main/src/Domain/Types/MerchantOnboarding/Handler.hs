{-# LANGUAGE DeriveGeneric #-}

module Domain.Types.MerchantOnboarding.Handler where

import Data.Aeson (Value)
import qualified Data.Map as Map
import qualified Domain.Types.MerchantOnboarding as MO
import qualified Domain.Types.MerchantOnboardingStep as MOS
import Environment
import Kernel.Prelude
import Kernel.Types.Id

data StepHandler = StepHandler
  { stepHandlerName :: Text,
    validateAndProcess :: Id MOS.MerchantOnboardingStep -> Value -> Flow StepHandlerResult
  }

data StepHandlerResult = StepHandlerResult
  { success :: Bool,
    message :: Maybe Text,
    nextSteps :: [Text]
  }
  deriving (Generic, Show)

newtype HandlerRegistry = HandlerRegistry
  { handlers :: Map.Map (MO.OnboardingType, Text) StepHandler
  }
