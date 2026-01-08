{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BookingUpdateRequest where

import qualified Database.Beam as B
import qualified Domain.Types.BookingUpdateRequest
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data BookingUpdateRequestT f = BookingUpdateRequestT
  { bapBookingUpdateRequestId :: (B.C f Kernel.Prelude.Text),
    bookingId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currentPointLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    currentPointLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    estimatedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    fareParamsId :: (B.C f Kernel.Prelude.Text),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    getRouteReq :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    maxEstimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    oldEstimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    oldEstimatedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    oldFareParamsId :: (B.C f Kernel.Prelude.Text),
    oldMaxEstimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    routeInfoResp :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    snapToRoadFailed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    status :: (B.C f Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus),
    totalDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    travelledDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingUpdateRequestT where
  data PrimaryKey BookingUpdateRequestT f = BookingUpdateRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingUpdateRequestId . id

type BookingUpdateRequest = BookingUpdateRequestT Identity

$(enableKVPG (''BookingUpdateRequestT) [('id)] [[('bapBookingUpdateRequestId)], [('bookingId)]])

$(mkTableInstancesWithTModifier (''BookingUpdateRequestT) "booking_update_request" [])
