{-# LANGUAGE DeriveGeneric #-}

module Domain.Action.Dashboard.AppManagement.MerchantOnboarding.Handlers where

import Data.Aeson
import qualified Data.Map as Map
import qualified Domain.Types.MerchantOnboarding as MO
import qualified Domain.Types.MerchantOnboarding.Handler as H
import Domain.Types.MerchantOnboardingStep as MOS
import Environment
import Kernel.Prelude
import Kernel.Types.Id

handleMerchantDetails :: Id MOS.MerchantOnboardingStep -> Value -> Flow H.StepHandlerResult
handleMerchantDetails _stepId _payload = do
  -- Add your business logic here --
  pure
    H.StepHandlerResult
      { success = True,
        message = Just "Merchant details validated successfully",
        nextSteps = ["document_verification"]
      }

handlerRegistry :: H.HandlerRegistry
handlerRegistry =
  H.HandlerRegistry
    { handlers =
        Map.fromList
          [ ((MO.TICKET_MERCHANT_ONBOARDING, "merchant_details"), H.StepHandler "merchant_details" handleMerchantDetails)
          ]
    }
