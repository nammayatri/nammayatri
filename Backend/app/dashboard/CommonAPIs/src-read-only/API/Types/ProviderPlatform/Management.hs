{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management where

import qualified API.Types.ProviderPlatform.Management.Account
import qualified API.Types.ProviderPlatform.Management.Booking
import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import qualified API.Types.ProviderPlatform.Management.Driver
import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified API.Types.ProviderPlatform.Management.EntityInfo
import qualified API.Types.ProviderPlatform.Management.Media
import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified API.Types.ProviderPlatform.Management.Message
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified API.Types.ProviderPlatform.Management.Ride
import qualified API.Types.ProviderPlatform.Management.System
import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import qualified API.Types.ProviderPlatform.Management.Volunteer
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data ManagementUserActionType
  = ACCOUNT API.Types.ProviderPlatform.Management.Account.AccountUserActionType
  | BOOKING API.Types.ProviderPlatform.Management.Booking.BookingUserActionType
  | COINS_CONFIG API.Types.ProviderPlatform.Management.CoinsConfig.CoinsConfigUserActionType
  | DRIVER API.Types.ProviderPlatform.Management.Driver.DriverUserActionType
  | DRIVER_COINS API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsUserActionType
  | DRIVER_GO_HOME API.Types.ProviderPlatform.Management.DriverGoHome.DriverGoHomeUserActionType
  | DRIVER_REFERRAL API.Types.ProviderPlatform.Management.DriverReferral.DriverReferralUserActionType
  | DRIVER_REGISTRATION API.Types.ProviderPlatform.Management.DriverRegistration.DriverRegistrationUserActionType
  | ENTITY_INFO API.Types.ProviderPlatform.Management.EntityInfo.EntityInfoUserActionType
  | MEDIA API.Types.ProviderPlatform.Management.Media.MediaUserActionType
  | MEDIA_FILE_DOCUMENT API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentUserActionType
  | MERCHANT API.Types.ProviderPlatform.Management.Merchant.MerchantUserActionType
  | MESSAGE API.Types.ProviderPlatform.Management.Message.MessageUserActionType
  | NAMMA_TAG API.Types.ProviderPlatform.Management.NammaTag.NammaTagUserActionType
  | PAYOUT API.Types.ProviderPlatform.Management.Payout.PayoutUserActionType
  | REVENUE API.Types.ProviderPlatform.Management.Revenue.RevenueUserActionType
  | RIDE API.Types.ProviderPlatform.Management.Ride.RideUserActionType
  | SYSTEM API.Types.ProviderPlatform.Management.System.SystemUserActionType
  | VEHICLE_INFO API.Types.ProviderPlatform.Management.VehicleInfo.VehicleInfoUserActionType
  | VOLUNTEER API.Types.ProviderPlatform.Management.Volunteer.VolunteerUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementUserActionType where
  show = \case
    ACCOUNT e -> "ACCOUNT/" <> show e
    BOOKING e -> "BOOKING/" <> show e
    COINS_CONFIG e -> "COINS_CONFIG/" <> show e
    DRIVER e -> "DRIVER/" <> show e
    DRIVER_COINS e -> "DRIVER_COINS/" <> show e
    DRIVER_GO_HOME e -> "DRIVER_GO_HOME/" <> show e
    DRIVER_REFERRAL e -> "DRIVER_REFERRAL/" <> show e
    DRIVER_REGISTRATION e -> "DRIVER_REGISTRATION/" <> show e
    ENTITY_INFO e -> "ENTITY_INFO/" <> show e
    MEDIA e -> "MEDIA/" <> show e
    MEDIA_FILE_DOCUMENT e -> "MEDIA_FILE_DOCUMENT/" <> show e
    MERCHANT e -> "MERCHANT/" <> show e
    MESSAGE e -> "MESSAGE/" <> show e
    NAMMA_TAG e -> "NAMMA_TAG/" <> show e
    PAYOUT e -> "PAYOUT/" <> show e
    REVENUE e -> "REVENUE/" <> show e
    RIDE e -> "RIDE/" <> show e
    SYSTEM e -> "SYSTEM/" <> show e
    VEHICLE_INFO e -> "VEHICLE_INFO/" <> show e
    VOLUNTEER e -> "VOLUNTEER/" <> show e

instance Text.Read.Read ManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(ACCOUNT v1, r2) | r1 <- stripPrefix "ACCOUNT/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( BOOKING v1,
                   r2
                 )
                 | r1 <- stripPrefix "BOOKING/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( COINS_CONFIG v1,
                   r2
                 )
                 | r1 <- stripPrefix "COINS_CONFIG/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DRIVER v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DRIVER_COINS v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_COINS/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DRIVER_GO_HOME v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_GO_HOME/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DRIVER_REFERRAL v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_REFERRAL/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( DRIVER_REGISTRATION v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_REGISTRATION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( ENTITY_INFO v1,
                   r2
                 )
                 | r1 <- stripPrefix "ENTITY_INFO/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MEDIA v1,
                   r2
                 )
                 | r1 <- stripPrefix "MEDIA/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MEDIA_FILE_DOCUMENT v1,
                   r2
                 )
                 | r1 <- stripPrefix "MEDIA_FILE_DOCUMENT/" r,
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
            ++ [ ( MESSAGE v1,
                   r2
                 )
                 | r1 <- stripPrefix "MESSAGE/" r,
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
            ++ [ ( PAYOUT v1,
                   r2
                 )
                 | r1 <- stripPrefix "PAYOUT/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( REVENUE v1,
                   r2
                 )
                 | r1 <- stripPrefix "REVENUE/" r,
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
            ++ [ ( VEHICLE_INFO v1,
                   r2
                 )
                 | r1 <- stripPrefix "VEHICLE_INFO/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( VOLUNTEER v1,
                   r2
                 )
                 | r1 <- stripPrefix "VOLUNTEER/" r,
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
