{-# LANGUAGE DeriveGeneric #-}

module Domain.Action.Dashboard.AppManagement.MerchantOnboarding.Handlers where

import qualified Data.Map as Map
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding.TicketHandlers as TH
import qualified Domain.Types.MerchantOnboarding as MO
import qualified Domain.Types.MerchantOnboarding.Handler as H

handlerRegistry :: H.HandlerRegistry
handlerRegistry =
  H.HandlerRegistry
    { handlers =
        Map.fromList
          [ ((MO.TICKET_MERCHANT_ONBOARDING, "TICKET-MERCHANT-DETAILS-SUBMIT-HANDLER"), H.StepHandler "TICKET-MERCHANT-DETAILS-SUBMIT-HANDLER" TH.handleMerchantDetailsSubmit),
            ((MO.TICKET_MERCHANT_ONBOARDING, "TICKET-MERCHANT-DETAILS-APPROVAL-HANDLER"), H.StepHandler "TICKET-MERCHANT-DETAILS-APPROVAL-HANDLER" TH.handleMerchantDetailsApprove),
            ((MO.TICKET_MERCHANT_ONBOARDING, "BANK-ONBOARDING-APPROVAL-HANDLER"), H.StepHandler "BANK-ONBOARDING-APPROVAL-HANDLER" TH.handleBankOnboardingApproval)
          ]
    }
