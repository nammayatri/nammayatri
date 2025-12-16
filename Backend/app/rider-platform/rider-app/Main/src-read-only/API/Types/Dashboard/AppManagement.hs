{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement where

import qualified API.Types.Dashboard.AppManagement.Customer
import qualified API.Types.Dashboard.AppManagement.EventManagement
import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified API.Types.Dashboard.AppManagement.Pass
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data AppManagementUserActionType
  = CUSTOMER API.Types.Dashboard.AppManagement.Customer.CustomerUserActionType
  | EVENT_MANAGEMENT API.Types.Dashboard.AppManagement.EventManagement.EventManagementUserActionType
  | MERCHANT_ONBOARDING API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingUserActionType
  | PASS API.Types.Dashboard.AppManagement.Pass.PassUserActionType
  | TICKET_DASHBOARD API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardUserActionType
  | TICKETS API.Types.Dashboard.AppManagement.Tickets.TicketsUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show AppManagementUserActionType where
  show = \case
    CUSTOMER e -> "CUSTOMER/" <> show e
    EVENT_MANAGEMENT e -> "EVENT_MANAGEMENT/" <> show e
    MERCHANT_ONBOARDING e -> "MERCHANT_ONBOARDING/" <> show e
    PASS e -> "PASS/" <> show e
    TICKET_DASHBOARD e -> "TICKET_DASHBOARD/" <> show e
    TICKETS e -> "TICKETS/" <> show e

instance Text.Read.Read AppManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(CUSTOMER v1, r2) | r1 <- stripPrefix "CUSTOMER/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( EVENT_MANAGEMENT v1,
                   r2
                 )
                 | r1 <- stripPrefix "EVENT_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MERCHANT_ONBOARDING v1,
                   r2
                 )
                 | r1 <- stripPrefix "MERCHANT_ONBOARDING/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PASS v1,
                   r2
                 )
                 | r1 <- stripPrefix "PASS/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( TICKET_DASHBOARD v1,
                   r2
                 )
                 | r1 <- stripPrefix "TICKET_DASHBOARD/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( TICKETS v1,
                   r2
                 )
                 | r1 <- stripPrefix "TICKETS/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''AppManagementUserActionType])
