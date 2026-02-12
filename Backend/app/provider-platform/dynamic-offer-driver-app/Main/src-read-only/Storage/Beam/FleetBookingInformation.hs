{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetBookingInformation where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FleetBookingInformationT f = FleetBookingInformationT
  { amount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    bookedSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bookingId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerMobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customerMobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    customerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentMethod :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    personId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    placeName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketBookingServiceShortId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketBookingShortId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketPlaceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    visitDate :: B.C f (Kernel.Prelude.Maybe Data.Time.Day)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetBookingInformationT where
  data PrimaryKey FleetBookingInformationT f = FleetBookingInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetBookingInformationId . id

type FleetBookingInformation = FleetBookingInformationT Identity

$(enableKVPG ''FleetBookingInformationT ['id] [])

$(mkTableInstances ''FleetBookingInformationT "fleet_booking_information")
