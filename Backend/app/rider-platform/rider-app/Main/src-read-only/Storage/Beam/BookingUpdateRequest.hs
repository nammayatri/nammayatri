{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BookingUpdateRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.BookingUpdateRequest
import qualified Database.Beam as B



data BookingUpdateRequestT f
    = BookingUpdateRequestT {bookingId :: (B.C f Kernel.Prelude.Text),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             currentPointLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                             currentPointLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                             distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                             errorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                             errorMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                             estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
                             estimatedFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                             id :: (B.C f Kernel.Prelude.Text),
                             merchantId :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             oldEstimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
                             oldEstimatedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             status :: (B.C f Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus),
                             totalDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
                             travelledDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BookingUpdateRequestT
    where data PrimaryKey BookingUpdateRequestT f = BookingUpdateRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BookingUpdateRequestId . id
type BookingUpdateRequest = BookingUpdateRequestT Identity

$(enableKVPG (''BookingUpdateRequestT) [('id)] [[('bookingId)]])

$(mkTableInstancesWithTModifier (''BookingUpdateRequestT) "booking_update_request" [])

