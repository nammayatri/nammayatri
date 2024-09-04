{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Booking where

import qualified Database.Beam as B
import qualified Domain.Types.Booking
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { area :: B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area),
    bapCity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City),
    bapCountry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country),
    bapId :: B.C f Kernel.Prelude.Text,
    bapUri :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceToPickup :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    estimateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    fareParametersId :: B.C f Kernel.Prelude.Text,
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasIntermediateStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    initiatedAs :: B.C f (Kernel.Prelude.Maybe Domain.Types.Trip.TripParty),
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDashboardRequest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxEstimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    primaryExophone :: B.C f Kernel.Prelude.Text,
    providerId :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f Kernel.Prelude.Text,
    receiverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    receiverName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    receiverPrimaryExophone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    returnTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    riderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    roundTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    senderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    senderName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    senderPrimaryExophone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialZoneOtpCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.Booking.BookingStatus,
    stopLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    transactionId :: B.C f Kernel.Prelude.Text,
    bookingType :: B.C f Domain.Types.Booking.BookingType,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleVariant :: B.C f Domain.Types.Common.ServiceTierType,
    vehicleServiceTierAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleServiceTierSeatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f = BookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingId . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['quoteId], ['specialZoneOtpCode], ['transactionId]])

$(mkTableInstances ''BookingT "booking")
