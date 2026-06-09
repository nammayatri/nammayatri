{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement where

import qualified API.Types.Dashboard.AppManagement.Customer
import qualified API.Types.Dashboard.AppManagement.EDCMachine
import qualified API.Types.Dashboard.AppManagement.EventManagement
import qualified API.Types.Dashboard.AppManagement.FRFSTicketService
import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified API.Types.Dashboard.AppManagement.Pass
import qualified API.Types.Dashboard.AppManagement.PassOrganization
import qualified API.Types.Dashboard.AppManagement.Passetto
import qualified API.Types.Dashboard.AppManagement.Payment
import qualified API.Types.Dashboard.AppManagement.StopRouteDetails
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified API.Types.Dashboard.AppManagement.TransitOperator
import qualified API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data AppManagementUserActionType
  = CUSTOMER API.Types.Dashboard.AppManagement.Customer.CustomerUserActionType
  | EDC_MACHINE API.Types.Dashboard.AppManagement.EDCMachine.EDCMachineUserActionType
  | EVENT_MANAGEMENT API.Types.Dashboard.AppManagement.EventManagement.EventManagementUserActionType
  | FRFS_TICKET_SERVICE API.Types.Dashboard.AppManagement.FRFSTicketService.FRFSTicketServiceUserActionType
  | MERCHANT_ONBOARDING API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingUserActionType
  | PASS API.Types.Dashboard.AppManagement.Pass.PassUserActionType
  | PASS_ORGANIZATION API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUserActionType
  | PASSETTO API.Types.Dashboard.AppManagement.Passetto.PassettoUserActionType
  | PAYMENT API.Types.Dashboard.AppManagement.Payment.PaymentUserActionType
  | STOP_ROUTE_DETAILS API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsUserActionType
  | TICKET_DASHBOARD API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardUserActionType
  | TICKETS API.Types.Dashboard.AppManagement.Tickets.TicketsUserActionType
  | TRANSIT_OPERATOR API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUserActionType
  | VEHICLE_SEAT_LAYOUT_MAPPING API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show AppManagementUserActionType where
  show = \case
    CUSTOMER e -> "CUSTOMER/" <> show e
    EDC_MACHINE e -> "EDC_MACHINE/" <> show e
    EVENT_MANAGEMENT e -> "EVENT_MANAGEMENT/" <> show e
    FRFS_TICKET_SERVICE e -> "FRFS_TICKET_SERVICE/" <> show e
    MERCHANT_ONBOARDING e -> "MERCHANT_ONBOARDING/" <> show e
    PASS e -> "PASS/" <> show e
    PASS_ORGANIZATION e -> "PASS_ORGANIZATION/" <> show e
    PASSETTO e -> "PASSETTO/" <> show e
    PAYMENT e -> "PAYMENT/" <> show e
    STOP_ROUTE_DETAILS e -> "STOP_ROUTE_DETAILS/" <> show e
    TICKET_DASHBOARD e -> "TICKET_DASHBOARD/" <> show e
    TICKETS e -> "TICKETS/" <> show e
    TRANSIT_OPERATOR e -> "TRANSIT_OPERATOR/" <> show e
    VEHICLE_SEAT_LAYOUT_MAPPING e -> "VEHICLE_SEAT_LAYOUT_MAPPING/" <> show e

instance Text.Read.Read AppManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(CUSTOMER v1, r2) | r1 <- stripPrefix "CUSTOMER/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( EDC_MACHINE v1,
                   r2
                 )
                 | r1 <- stripPrefix "EDC_MACHINE/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( EVENT_MANAGEMENT v1,
                   r2
                 )
                 | r1 <- stripPrefix "EVENT_MANAGEMENT/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( FRFS_TICKET_SERVICE v1,
                   r2
                 )
                 | r1 <- stripPrefix "FRFS_TICKET_SERVICE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
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
            ++ [ ( PASS_ORGANIZATION v1,
                   r2
                 )
                 | r1 <- stripPrefix "PASS_ORGANIZATION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PASSETTO v1,
                   r2
                 )
                 | r1 <- stripPrefix "PASSETTO/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PAYMENT v1,
                   r2
                 )
                 | r1 <- stripPrefix "PAYMENT/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( STOP_ROUTE_DETAILS v1,
                   r2
                 )
                 | r1 <- stripPrefix "STOP_ROUTE_DETAILS/" r,
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
            ++ [ ( TRANSIT_OPERATOR v1,
                   r2
                 )
                 | r1 <- stripPrefix "TRANSIT_OPERATOR/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( VEHICLE_SEAT_LAYOUT_MAPPING v1,
                   r2
                 )
                 | r1 <- stripPrefix "VEHICLE_SEAT_LAYOUT_MAPPING/" r,
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
