{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BookingPartiesLink where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.Trip
import qualified Database.Beam as B



data BookingPartiesLinkT f
    = BookingPartiesLinkT {bookingId :: (B.C f Kernel.Prelude.Text),
                           id :: (B.C f Kernel.Prelude.Text),
                           isActive :: (B.C f Kernel.Prelude.Bool),
                           partyId :: (B.C f Kernel.Prelude.Text),
                           partyName :: (B.C f Kernel.Prelude.Text),
                           partyType :: (B.C f Domain.Types.Trip.TripParty),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BookingPartiesLinkT
    where data PrimaryKey BookingPartiesLinkT f = BookingPartiesLinkId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BookingPartiesLinkId . id
type BookingPartiesLink = BookingPartiesLinkT Identity

$(enableKVPG (''BookingPartiesLinkT) [('id)] [[('bookingId)], [('partyId)]])

$(mkTableInstances (''BookingPartiesLinkT) "booking_parties_link")

