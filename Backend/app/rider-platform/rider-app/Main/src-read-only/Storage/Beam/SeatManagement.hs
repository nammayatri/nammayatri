{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SeatManagement where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Time
import qualified Database.Beam as B



data SeatManagementT f
    = SeatManagementT {blocked :: (B.C f Kernel.Prelude.Int),
                       booked :: (B.C f Kernel.Prelude.Int),
                       date :: (B.C f Data.Time.Day),
                       id :: (B.C f Kernel.Prelude.Text),
                       maxCapacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       ticketServiceCategoryId :: (B.C f Kernel.Prelude.Text),
                       merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                       merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                       createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SeatManagementT
    where data PrimaryKey SeatManagementT f = SeatManagementId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SeatManagementId . id
type SeatManagement = SeatManagementT Identity

$(enableKVPG (''SeatManagementT) [('id)] [[('ticketServiceCategoryId)]])

$(mkTableInstances (''SeatManagementT) "seat_management")

