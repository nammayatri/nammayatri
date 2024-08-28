{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management where

import qualified API.Types.ProviderPlatform.Management.Booking
import qualified API.Types.ProviderPlatform.Management.Driver
import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified API.Types.ProviderPlatform.Management.Message
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Data.List
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementEndpoint
  = BookingAPI API.Types.ProviderPlatform.Management.Booking.BookingEndpointDSL
  | DriverAPI API.Types.ProviderPlatform.Management.Driver.DriverEndpointDSL
  | DriverCoinsAPI API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsEndpointDSL
  | DriverGoHomeAPI API.Types.ProviderPlatform.Management.DriverGoHome.DriverGoHomeEndpointDSL
  | DriverReferralAPI API.Types.ProviderPlatform.Management.DriverReferral.DriverReferralEndpointDSL
  | DriverRegistrationAPI API.Types.ProviderPlatform.Management.DriverRegistration.DriverRegistrationEndpointDSL
  | MerchantAPI API.Types.ProviderPlatform.Management.Merchant.MerchantEndpointDSL
  | MessageAPI API.Types.ProviderPlatform.Management.Message.MessageEndpointDSL
  | NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.NammaTagEndpointDSL
  | PayoutAPI API.Types.ProviderPlatform.Management.Payout.PayoutEndpointDSL
  | RevenueAPI API.Types.ProviderPlatform.Management.Revenue.RevenueEndpointDSL
  | RideAPI API.Types.ProviderPlatform.Management.Ride.RideEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementEndpoint where
  show = \case
    BookingAPI e -> "BookingAPI_" <> show e
    DriverAPI e -> "DriverAPI_" <> show e
    DriverCoinsAPI e -> "DriverCoinsAPI_" <> show e
    DriverGoHomeAPI e -> "DriverGoHomeAPI_" <> show e
    DriverReferralAPI e -> "DriverReferralAPI_" <> show e
    DriverRegistrationAPI e -> "DriverRegistrationAPI_" <> show e
    MerchantAPI e -> "MerchantAPI_" <> show e
    MessageAPI e -> "MessageAPI_" <> show e
    NammaTagAPI e -> "NammaTagAPI_" <> show e
    PayoutAPI e -> "PayoutAPI_" <> show e
    RevenueAPI e -> "RevenueAPI_" <> show e
    RideAPI e -> "RideAPI_" <> show e

instance Text.Read.Read ManagementEndpoint where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BookingAPI v1, r2) | r1 <- stripPrefix "BookingAPI_" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( DriverAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DriverAPI_" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DriverCoinsAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DriverCoinsAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DriverGoHomeAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DriverGoHomeAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DriverReferralAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DriverReferralAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DriverRegistrationAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DriverRegistrationAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
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
            ++ [ ( MessageAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "MessageAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( NammaTagAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "NammaTagAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PayoutAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "PayoutAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( RevenueAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "RevenueAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( RideAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "RideAPI_" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
