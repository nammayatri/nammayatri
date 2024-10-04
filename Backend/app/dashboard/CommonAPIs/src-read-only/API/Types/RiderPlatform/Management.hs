{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management where

import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementUserActionType
  = BOOKING API.Types.RiderPlatform.Management.Booking.BookingUserActionType
  | INVOICE API.Types.RiderPlatform.Management.Invoice.InvoiceUserActionType
  | MERCHANT API.Types.RiderPlatform.Management.Merchant.MerchantUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementUserActionType where
  show = \case
    BOOKING e -> "BOOKING/" <> show e
    INVOICE e -> "INVOICE/" <> show e
    MERCHANT e -> "MERCHANT/" <> show e

instance Text.Read.Read ManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BOOKING v1, r2) | r1 <- stripPrefix "BOOKING/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( INVOICE v1,
                   r2
                 )
                 | r1 <- stripPrefix "INVOICE/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
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
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''ManagementUserActionType])
