{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BookingUpdateRequest where

import qualified Database.Beam as B
import qualified Domain.Types.BookingUpdateRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data BookingUpdateRequestT f = BookingUpdateRequestT
  { id :: B.C f Kernel.Prelude.Text,
    bookingId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus,
    travelledDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    estimatedFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    currentPointLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    currentPointLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    oldEstimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    oldEstimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    errorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingUpdateRequestT where
  data PrimaryKey BookingUpdateRequestT f = BookingUpdateRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingUpdateRequestId . id

type BookingUpdateRequest = BookingUpdateRequestT Identity

$(enableKVPG ''BookingUpdateRequestT ['id] [['bookingId]])

$(mkTableInstancesWithTModifier ''BookingUpdateRequestT "booking_update_request" [])
