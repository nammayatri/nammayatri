{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Seat where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.Seat
import qualified Database.Beam as B



data SeatT f
    = SeatT {colNo :: (B.C f Kernel.Prelude.Int),
             directionDegrees :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
             id :: (B.C f Kernel.Prelude.Text),
             isBookable :: (B.C f Kernel.Prelude.Bool),
             isLadiesOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
             merchantId :: (B.C f Kernel.Prelude.Text),
             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
             minStopsRequired :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
             rowNo :: (B.C f Kernel.Prelude.Int),
             seatLabel :: (B.C f Kernel.Prelude.Text),
             seatLayoutId :: (B.C f Kernel.Prelude.Text),
             seatType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Seat.SeatType)),
             createdAt :: (B.C f Kernel.Prelude.UTCTime),
             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SeatT
    where data PrimaryKey SeatT f = SeatId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SeatId . id
type Seat = SeatT Identity

$(enableKVPG (''SeatT) [('id)] [[('seatLayoutId)]])

$(mkTableInstances (''SeatT) "seat")

