{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingPartiesLink where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Person
import qualified Domain.Types.Trip
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BookingPartiesLink = BookingPartiesLink
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    id :: Kernel.Types.Id.Id Domain.Types.BookingPartiesLink.BookingPartiesLink,
    isActive :: Kernel.Prelude.Bool,
    partyId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    partyName :: Kernel.Prelude.Text,
    partyType :: Domain.Types.Trip.TripParty,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
