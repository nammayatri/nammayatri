{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management where

import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common
import qualified Data.List
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementEndpoint
  = BookingAPI API.Types.RiderPlatform.Management.Booking.BookingEndpointDSL
  | FRFSTicketAPI API.Types.RiderPlatform.Management.FRFSTicket.FRFSTicketEndpointDSL
  | InvoiceAPI API.Types.RiderPlatform.Management.Invoice.InvoiceEndpointDSL
  | MerchantAPI API.Types.RiderPlatform.Management.Merchant.MerchantEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementEndpoint where
  show = \case
    BookingAPI e -> "BOOKING/" <> Dashboard.Common.showUserActionType e
    FRFSTicketAPI e -> "FRFS_TICKET/" <> Dashboard.Common.showUserActionType e
    InvoiceAPI e -> "INVOICE/" <> Dashboard.Common.showUserActionType e
    MerchantAPI e -> "MERCHANT/" <> Dashboard.Common.showUserActionType e

instance Text.Read.Read ManagementEndpoint where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BookingAPI v1, r2) | r1 <- stripPrefix "BOOKING/" r, (v1, r2) <- Dashboard.Common.readUserActionTypeS r1]
            ++ [ ( FRFSTicketAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "FRFS_TICKET/" r,
                   (v1, r2) <- Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( InvoiceAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "INVOICE/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( MerchantAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "MERCHANT/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
