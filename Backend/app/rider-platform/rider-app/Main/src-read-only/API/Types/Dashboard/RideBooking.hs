{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking where

import qualified API.Types.Dashboard.RideBooking.Booking
import qualified API.Types.Dashboard.RideBooking.Cancel
import qualified API.Types.Dashboard.RideBooking.Confirm
import qualified API.Types.Dashboard.RideBooking.Frontend
import qualified API.Types.Dashboard.RideBooking.Maps
import qualified API.Types.Dashboard.RideBooking.MultiModal
import qualified API.Types.Dashboard.RideBooking.NotifyRideInfo
import qualified API.Types.Dashboard.RideBooking.Profile
import qualified API.Types.Dashboard.RideBooking.Quote
import qualified API.Types.Dashboard.RideBooking.Registration
import qualified API.Types.Dashboard.RideBooking.Search
import qualified API.Types.Dashboard.RideBooking.Select
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data RideBookingUserActionType
  = BOOKING API.Types.Dashboard.RideBooking.Booking.BookingUserActionType
  | CANCEL API.Types.Dashboard.RideBooking.Cancel.CancelUserActionType
  | CONFIRM API.Types.Dashboard.RideBooking.Confirm.ConfirmUserActionType
  | FRONTEND API.Types.Dashboard.RideBooking.Frontend.FrontendUserActionType
  | MAPS API.Types.Dashboard.RideBooking.Maps.MapsUserActionType
  | MULTI_MODAL API.Types.Dashboard.RideBooking.MultiModal.MultiModalUserActionType
  | NOTIFY_RIDE_INFO API.Types.Dashboard.RideBooking.NotifyRideInfo.NotifyRideInfoUserActionType
  | PROFILE API.Types.Dashboard.RideBooking.Profile.ProfileUserActionType
  | QUOTE API.Types.Dashboard.RideBooking.Quote.QuoteUserActionType
  | REGISTRATION API.Types.Dashboard.RideBooking.Registration.RegistrationUserActionType
  | SEARCH API.Types.Dashboard.RideBooking.Search.SearchUserActionType
  | SELECT API.Types.Dashboard.RideBooking.Select.SelectUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show RideBookingUserActionType where
  show = \case
    BOOKING e -> "BOOKING/" <> show e
    CANCEL e -> "CANCEL/" <> show e
    CONFIRM e -> "CONFIRM/" <> show e
    FRONTEND e -> "FRONTEND/" <> show e
    MAPS e -> "MAPS/" <> show e
    MULTI_MODAL e -> "MULTI_MODAL/" <> show e
    NOTIFY_RIDE_INFO e -> "NOTIFY_RIDE_INFO/" <> show e
    PROFILE e -> "PROFILE/" <> show e
    QUOTE e -> "QUOTE/" <> show e
    REGISTRATION e -> "REGISTRATION/" <> show e
    SEARCH e -> "SEARCH/" <> show e
    SELECT e -> "SELECT/" <> show e

instance Text.Read.Read RideBookingUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(BOOKING v1, r2) | r1 <- stripPrefix "BOOKING/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( CANCEL v1,
                   r2
                 )
                 | r1 <- stripPrefix "CANCEL/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( CONFIRM v1,
                   r2
                 )
                 | r1 <- stripPrefix "CONFIRM/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( FRONTEND v1,
                   r2
                 )
                 | r1 <- stripPrefix "FRONTEND/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MAPS v1,
                   r2
                 )
                 | r1 <- stripPrefix "MAPS/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( MULTI_MODAL v1,
                   r2
                 )
                 | r1 <- stripPrefix "MULTI_MODAL/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( NOTIFY_RIDE_INFO v1,
                   r2
                 )
                 | r1 <- stripPrefix "NOTIFY_RIDE_INFO/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PROFILE v1,
                   r2
                 )
                 | r1 <- stripPrefix "PROFILE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( QUOTE v1,
                   r2
                 )
                 | r1 <- stripPrefix "QUOTE/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( REGISTRATION v1,
                   r2
                 )
                 | r1 <- stripPrefix "REGISTRATION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( SEARCH v1,
                   r2
                 )
                 | r1 <- stripPrefix "SEARCH/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( SELECT v1,
                   r2
                 )
                 | r1 <- stripPrefix "SELECT/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''RideBookingUserActionType])
