{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management where

import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.Customer
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified API.Types.RiderPlatform.Management.NammaTag
import qualified API.Types.RiderPlatform.Management.Ride
import qualified API.Types.RiderPlatform.Management.System
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementUserActionType
  = BOOKING API.Types.RiderPlatform.Management.Booking.BookingUserActionType
  | CUSTOMER API.Types.RiderPlatform.Management.Customer.CustomerUserActionType
  | FRFS_TICKET API.Types.RiderPlatform.Management.FRFSTicket.FRFSTicketUserActionType
  | INVOICE API.Types.RiderPlatform.Management.Invoice.InvoiceUserActionType
  | MERCHANT API.Types.RiderPlatform.Management.Merchant.MerchantUserActionType
  | NAMMA_TAG API.Types.RiderPlatform.Management.NammaTag.NammaTagUserActionType
  | RIDE API.Types.RiderPlatform.Management.Ride.RideUserActionType
  | SYSTEM API.Types.RiderPlatform.Management.System.SystemUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementUserActionType where
  show = \case
    BOOKING e -> "BOOKING/" <> show e
    CUSTOMER e -> "CUSTOMER/" <> show e
    FRFS_TICKET e -> "FRFS_TICKET/" <> show e
    INVOICE e -> "INVOICE/" <> show e
    MERCHANT e -> "MERCHANT/" <> show e
    NAMMA_TAG e -> "NAMMA_TAG/" <> show e
    RIDE e -> "RIDE/" <> show e
    SYSTEM e -> "SYSTEM/" <> show e

instance Text.Read.Read ManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BOOKING v1, r2) | r1 <- stripPrefix "BOOKING/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( CUSTOMER v1,
                   r2
                 )
                 | r1 <- stripPrefix "CUSTOMER/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( FRFS_TICKET v1,
                   r2
                 )
                 | r1 <- stripPrefix "FRFS_TICKET/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( INVOICE v1,
                   r2
                 )
                 | r1 <- stripPrefix "INVOICE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MERCHANT v1,
                   r2
                 )
                 | r1 <- stripPrefix "MERCHANT/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( NAMMA_TAG v1,
                   r2
                 )
                 | r1 <- stripPrefix "NAMMA_TAG/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( RIDE v1,
                   r2
                 )
                 | r1 <- stripPrefix "RIDE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( SYSTEM v1,
                   r2
                 )
                 | r1 <- stripPrefix "SYSTEM/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''ManagementUserActionType)])
