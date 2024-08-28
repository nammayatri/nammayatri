{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management where

import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Data.List
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementEndpoint
  = BookingAPI API.Types.RiderPlatform.Management.Booking.BookingEndpointDSL
  | InvoiceAPI API.Types.RiderPlatform.Management.Invoice.InvoiceEndpointDSL
  | MerchantAPI API.Types.RiderPlatform.Management.Merchant.MerchantEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementEndpoint where
  show = \case
    BookingAPI e -> "BookingAPI_" <> show e
    InvoiceAPI e -> "InvoiceAPI_" <> show e
    MerchantAPI e -> "MerchantAPI_" <> show e

instance Text.Read.Read ManagementEndpoint where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BookingAPI v1, r2) | r1 <- stripPrefix "BookingAPI_" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( InvoiceAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "InvoiceAPI_" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MerchantAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "MerchantAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
