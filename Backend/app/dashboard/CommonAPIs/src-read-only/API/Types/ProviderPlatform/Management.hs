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
import qualified API.Types.ProviderPlatform.Management.System
import qualified Dashboard.Common
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
  | SystemAPI API.Types.ProviderPlatform.Management.System.SystemEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementEndpoint where
  show = \case
    BookingAPI e -> "BOOKING/" <> Dashboard.Common.showUserActionType e
    DriverAPI e -> "DRIVER/" <> Dashboard.Common.showUserActionType e
    DriverCoinsAPI e -> "DRIVER_COINS/" <> Dashboard.Common.showUserActionType e
    DriverGoHomeAPI e -> "DRIVER_GO_HOME/" <> Dashboard.Common.showUserActionType e
    DriverReferralAPI e -> "DRIVER_REFERRAL/" <> Dashboard.Common.showUserActionType e
    DriverRegistrationAPI e -> "DRIVER_REGISTRATION/" <> Dashboard.Common.showUserActionType e
    MerchantAPI e -> "MERCHANT/" <> Dashboard.Common.showUserActionType e
    MessageAPI e -> "MESSAGE/" <> Dashboard.Common.showUserActionType e
    NammaTagAPI e -> "NAMMA_TAG/" <> Dashboard.Common.showUserActionType e
    PayoutAPI e -> "PAYOUT/" <> Dashboard.Common.showUserActionType e
    RevenueAPI e -> "REVENUE/" <> Dashboard.Common.showUserActionType e
    RideAPI e -> "RIDE/" <> Dashboard.Common.showUserActionType e
    SystemAPI e -> "SYSTEM/" <> Dashboard.Common.showUserActionType e

instance Text.Read.Read ManagementEndpoint where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BookingAPI v1, r2) | r1 <- stripPrefix "BOOKING/" r, (v1, r2) <- Dashboard.Common.readUserActionTypeS r1]
            ++ [ ( DriverAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER/" r,
                   (v1, r2) <- Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( DriverCoinsAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_COINS/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( DriverGoHomeAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_GO_HOME/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( DriverReferralAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_REFERRAL/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( DriverRegistrationAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_REGISTRATION/" r,
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
            ++ [ ( MessageAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "MESSAGE/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( NammaTagAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "NAMMA_TAG/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( PayoutAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "PAYOUT/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( RevenueAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "REVENUE/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( RideAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "RIDE/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
            ++ [ ( SystemAPI v1,
                   r2
                 )
                 | r1 <- stripPrefix "SYSTEM/" r,
                   ( v1,
                     r2
                     ) <-
                     Dashboard.Common.readUserActionTypeS r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
