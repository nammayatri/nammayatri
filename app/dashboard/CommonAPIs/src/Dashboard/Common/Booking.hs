{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Booking
  ( module Dashboard.Common.Booking,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data BookingEndpoint
  = StuckBookingsCancelEndpoint
  deriving (Show, Read)

derivePersistField "BookingEndpoint"

---------------------------------------------------------
-- bookings cancel --------------------------------------

type StuckBookingsCancelAPI =
  "cancel"
    :> "allStuck"
    :> ReqBody '[JSON] StuckBookingsCancelReq
    :> Post '[JSON] StuckBookingsCancelRes

newtype StuckBookingsCancelReq = StuckBookingsCancelReq
  { bookingIds :: [Id Booking]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype StuckBookingsCancelRes = StuckBookingsCancelRes
  { cancelledBookings :: [StuckBookingItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StuckBookingItem = StuckBookingItem
  { bookingId :: Id Booking,
    rideId :: Maybe (Id Ride)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

bookingStuckCode :: CancellationReasonCode
bookingStuckCode = CancellationReasonCode "BOOKING_NEW_STATUS_MORE_THAN_6HRS"

rideStuckCode :: CancellationReasonCode
rideStuckCode = CancellationReasonCode "RIDE_NEW_STATUS_MORE_THAN_6HRS"

instance HideSecrets StuckBookingsCancelReq where
  hideSecrets = identity

instance HideSecrets StuckBookingsCancelRes where
  hideSecrets = identity
